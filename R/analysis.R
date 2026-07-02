# R/analysis.R ----------------------------------------------------------------
# Shared analysis layer for the revamped app ("A Study in Breach").
#
# This file holds the single source of truth for how each abnormality is scored,
# so the Case Board overview and the deep-dive tabs always agree. It also holds
# small UI helpers used across the new tabs.
#
# Four abnormality signals, one per round. Three of them measure against a shared
# baseline period (the baseline split); Bypass is structural and needs no baseline.
#   Channel drift (Activity) - messages on a channel an agent barely used in baseline.
#   Bypass                   - public posts the Judge could not have cleared (%).
#   Network                  - agent-to-agent links not seen during the baseline period.
#   Topic                    - topic mentions that spike above their baseline-period rate.
#
# Each score is normalised to 0-1 within its own signal so the matrix can shade
# every lane on a common colour scale.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Internal dimension keys (used for routing) and their display labels.
dim_display <- c(Activity = "Channel drift", Bypass = "Bypass",
                 Network  = "Network",       Topic  = "Topic",
                 Overall  = "Overall")

# A sensible default baseline-period end: the last round the data labels "Baseline",
# else two-thirds of the way through the rounds.
default_split <- function(df = messages_tbl) {
  if ("period" %in% names(df)) {
    b <- df |> dplyr::filter(tolower(as.character(period)) == "baseline")
    if (nrow(b) > 0) return(max(b$round_idx))
  }
  max(2, round(max(df$round_idx) * 2 / 3))
}

# Suggest a baseline end: the round just before outside posting (public,
# unwatched channels) first appears, which marks where the breach begins. Falls
# back to the data's labelled baseline end.
suggest_baseline <- function(df = messages_tbl) {
  pub <- df |> dplyr::filter(channel %in% public_unmonitored)
  if (nrow(pub) > 0) {
    first_pub <- as.integer(min(pub$round_idx))
    return(max(2L, min(first_pub - 1L, as.integer(max(df$round_idx)) - 1L)))
  }
  default_split(df)
}

# --- Round -> date labels -----------------------------------------------------
# The packaged data carries a real timestamp per round (round_hour_dt). Uploaded
# data may not, so fall back to "R{n}" when no date is present.
round_label_tbl <- function(df = messages_tbl) {
  base <- df |> dplyr::distinct(round_idx) |> dplyr::arrange(round_idx)
  if ("round_hour_dt" %in% names(df) && any(!is.na(df$round_hour_dt))) {
    dts <- df |>
      dplyr::group_by(round_idx) |>
      dplyr::summarise(dt = dplyr::first(round_hour_dt), .groups = "drop")
    base <- base |>
      dplyr::left_join(dts, by = "round_idx") |>
      dplyr::mutate(lab = dplyr::if_else(is.na(dt),
                                         paste0("R", round_idx),
                                         format(dt, "%b %d %H:%M")))
  } else {
    base <- base |> dplyr::mutate(lab = paste0("R", round_idx))
  }
  base |> dplyr::select(round_idx, lab)
}

# A short date-span caption for chart footers, or "" when the data has no dates.
round_date_caption <- function(df = messages_tbl) {
  if (!("round_hour_dt" %in% names(df)) || all(is.na(df$round_hour_dt))) return("")
  rng <- range(df$round_hour_dt, na.rm = TRUE)
  paste0(format(rng[1], "%d %b %Y"), " to ", format(rng[2], "%d %b %Y"))
}

# --- Agent-to-agent edges for one slice of messages ---------------------------
# Self-contained copy so the analysis layer does not depend on section files.
# Two kinds of agent-to-agent links are combined: explicit named recipients, and
# replies (responding_to) resolved to the author of the message replied to. The
# reply edges matter because many internal messages record no named recipient, so
# without them the baseline rounds would look empty.
#
# Performance: the per-round edges and the message-author lookup do not change
# while the sliders move, so they are cached per dataset (see rebuild_edge_cache
# and round_edges). Recomputing them on every slider tick was the main lag.
edge_cache <- new.env(parent = emptyenv())

build_agent_edges <- function(df) {
  rec <- df |>
    dplyr::filter(!is.na(recipients_csv), recipients_csv != "",
                  recipients_csv != "ALL") |>
    tidyr::separate_rows(recipients_csv, sep = ";") |>
    dplyr::mutate(to = unname(recipient_to_agent[recipients_csv])) |>
    dplyr::filter(!is.na(to)) |>
    dplyr::transmute(from = as.character(agent_id), to = to)

  rep <- tibble::tibble(from = character(0), to = character(0))
  if ("responding_to" %in% names(df)) {
    id2agent <- if (!is.null(edge_cache$id2agent)) edge_cache$id2agent
                else setNames(as.character(messages_tbl$agent_id),
                              as.character(messages_tbl$message_id))
    rep <- df |>
      dplyr::filter(!is.na(responding_to), responding_to != "") |>
      dplyr::mutate(to = unname(id2agent[as.character(responding_to)])) |>
      dplyr::filter(!is.na(to), to != as.character(agent_id)) |>
      dplyr::transmute(from = as.character(agent_id), to = to)
  }

  dplyr::bind_rows(rec, rep) |>
    dplyr::count(from, to, name = "weight")
}

# Build the per-round edge cache and the message-author lookup once per dataset.
# Call after the data loads and after every apply_bundle().
rebuild_edge_cache <- function() {
  if (!exists("messages_tbl")) return(invisible(FALSE))
  edge_cache$id2agent <- setNames(as.character(messages_tbl$agent_id),
                                  as.character(messages_tbl$message_id))
  rds <- sort(unique(messages_tbl$round_idx))
  edge_cache$by_round <- setNames(
    lapply(rds, function(r)
      build_agent_edges(messages_tbl[messages_tbl$round_idx == r, , drop = FALSE])),
    as.character(rds))
  invisible(TRUE)
}

# Cached edges for one round (falls back to computing if the cache is cold).
round_edges <- function(r) {
  e <- if (!is.null(edge_cache$by_round)) edge_cache$by_round[[as.character(r)]] else NULL
  if (is.null(e))
    build_agent_edges(messages_tbl[messages_tbl$round_idx == r, , drop = FALSE])
  else e
}

# --- Signal 1: Channel drift -------------------------------------------------
# For each agent that spoke in a round, compare the share of that round's
# messages on each channel against the agent's baseline share for that channel.
# A cell "drifts" when the change from baseline is at least the cutoff (in
# percentage points) in EITHER direction: using a channel much more than usual
# (an increase) or much less than usual, including going silent on a channel it
# normally uses (a decrease). Only agents active in the round are scored, so true
# silence is not mistaken for drift.
drift_cells <- function(df, split, strict) {
  base <- agent_channel_baseline(df, split) |>
    dplyr::transmute(agent_id = as.character(agent_id),
                     channel = as.character(channel), s_base = share)
  active <- df |> dplyr::count(round_idx, agent_id, name = "round_total") |>
    dplyr::mutate(agent_id = as.character(agent_id))
  empty <- tibble::tibble(round_idx = integer(0), agent_id = character(0),
                          channel = character(0), n = integer(0),
                          s_round = numeric(0), s_base = numeric(0),
                          dev = numeric(0), drift = logical(0),
                          direction = character(0))
  if (nrow(active) == 0) return(empty)
  counts <- df |> dplyr::count(round_idx, agent_id, channel, name = "n") |>
    dplyr::mutate(agent_id = as.character(agent_id), channel = as.character(channel))
  active |>
    tidyr::crossing(channel = channel_hierarchy$channel) |>
    dplyr::left_join(counts, by = c("round_idx", "agent_id", "channel")) |>
    dplyr::mutate(n = tidyr::replace_na(n, 0), s_round = n / round_total) |>
    dplyr::left_join(base, by = c("agent_id", "channel")) |>
    dplyr::mutate(s_base = tidyr::replace_na(s_base, 0),
                  dev = s_round - s_base,
                  drift = abs(dev) >= strict / 100,
                  direction = dplyr::if_else(dev >= 0, "increase", "decrease"))
}

# Per-round channel-drift score: how many agent-channel cells drifted that round.
score_activity <- function(df, split, strict) {
  d <- drift_cells(df, split, strict)
  if (nrow(d) == 0) return(tibble::tibble(round_idx = integer(0), value = integer(0)))
  d |> dplyr::group_by(round_idx) |>
    dplyr::summarise(value = sum(drift), .groups = "drop")
}

# --- Signal 2: Bypass (semantic clearance) -----------------------------------
# Rather than a yes/no, each public unwatched post gets a clearance score: how
# closely its meaning matches something the same agent already said on a watched
# channel (this round or earlier). Matching is semantic, via latent semantic
# analysis (a TF-IDF term-document matrix reduced with SVD), so a post that
# rewords prior watched discussion still scores as cleared, while a post raising
# genuinely new content scores low. Bypass likelihood is 1 - clearance. If the
# corpus is too small for LSA, it falls back to plain word overlap.

# Tokenise message text into meaningful lower-case words.
.bp_tokens <- function(x) {
  toks <- unlist(strsplit(tolower(x %||% ""), "[^a-z]+"))
  toks <- toks[nchar(toks) >= 3]
  stop <- c("the","and","that","this","with","have","will","for","are","was",
            "but","not","you","your","our","their","they","from","has","had",
            "can","could","would","should","about","into","than","then","them",
            "its","what","which","who","there","here","been","being","were",
            "also","just","get","got","make","need","want","know","like","one",
            "all","any","may","more","most","some","such","very","out","now")
  toks[!toks %in% stop]
}

# Cosine similarity between two token vectors (0 to 1).
.bp_cosine <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  ta <- table(a); tb <- table(b)
  vocab <- union(names(ta), names(tb))
  va <- as.numeric(ta[vocab]); va[is.na(va)] <- 0
  vb <- as.numeric(tb[vocab]); vb[is.na(vb)] <- 0
  denom <- sqrt(sum(va^2)) * sqrt(sum(vb^2))
  if (denom == 0) 0 else sum(va * vb) / denom
}

# Build row-normalised latent-semantic vectors for a set of texts using TF-IDF
# plus truncated SVD. Returns an n x k matrix (rows aligned to `texts`) or NULL
# when the corpus is too small or thin for a meaningful reduction.
build_lsa <- function(texts, k = 50, max_terms = 1500, min_df = 2) {
  toks <- lapply(texts, .bp_tokens)
  dfreq_all <- table(unlist(lapply(toks, unique)))
  vocab <- names(dfreq_all)[dfreq_all >= min_df]
  if (length(vocab) < 5) return(NULL)
  if (length(vocab) > max_terms)
    vocab <- names(sort(dfreq_all[vocab], decreasing = TRUE))[seq_len(max_terms)]
  vidx <- setNames(seq_along(vocab), vocab)
  n <- length(texts); m <- length(vocab)
  M <- matrix(0, nrow = n, ncol = m)
  for (i in seq_len(n)) {
    tt <- toks[[i]]; tt <- tt[tt %in% vocab]
    if (length(tt)) { tb <- table(tt); M[i, vidx[names(tb)]] <- as.numeric(tb) }
  }
  idf <- log((1 + n) / (1 + colSums(M > 0))) + 1
  M <- sweep(M, 2, idf, `*`)
  kk <- min(k, m - 1L, n - 1L)
  if (kk < 2) return(NULL)
  sv <- tryCatch(svd(M, nu = kk, nv = 0), error = function(e) NULL)
  if (is.null(sv)) return(NULL)
  V <- sv$u[, seq_len(kk), drop = FALSE] %*% diag(sv$d[seq_len(kk)], kk, kk)
  nrm <- sqrt(rowSums(V^2)); nrm[nrm == 0] <- 1
  V / nrm
}

# One row per public unwatched post, with a clearance score (0 to 1), the bypass
# likelihood (1 - clearance), and the closest watched message that explains it.
bypass_clearance <- function(df) {
  posts <- df |> dplyr::filter(channel %in% public_unmonitored)
  if (nrow(posts) == 0) {
    return(dplyr::mutate(posts, clearance = numeric(0), bypass = numeric(0),
                         best_round = integer(0), best_match = character(0),
                         best_channel = character(0)))
  }
  content_all <- as.character(df$content)
  agent_all   <- as.character(df$agent_id)
  round_all   <- df$round_idx
  chan_all    <- as.character(df$channel)

  V <- tryCatch(build_lsa(content_all), error = function(e) NULL)
  tok_all <- if (is.null(V)) lapply(content_all, .bp_tokens) else NULL

  mon_mask  <- chan_all %in% monitored_channels
  post_rows <- which(chan_all %in% public_unmonitored)
  cl <- numeric(length(post_rows))
  br <- rep(NA_integer_, length(post_rows))
  bm <- rep(NA_character_, length(post_rows))
  bc <- rep(NA_character_, length(post_rows))
  for (ii in seq_along(post_rows)) {
    i <- post_rows[ii]
    cand <- which(mon_mask & agent_all == agent_all[i] & round_all <= round_all[i])
    if (length(cand) == 0) next
    if (!is.null(V)) {
      sims <- as.numeric(V[cand, , drop = FALSE] %*% V[i, ])
    } else {
      pt <- tok_all[[i]]
      sims <- vapply(cand, function(j) .bp_cosine(pt, tok_all[[j]]), numeric(1))
    }
    sims[is.na(sims)] <- 0
    k <- which.max(sims)
    cl[ii] <- max(0, sims[k])
    br[ii] <- as.integer(round_all[cand[k]])
    bm[ii] <- content_all[cand[k]]
    bc[ii] <- chan_all[cand[k]]
  }
  posts |>
    dplyr::mutate(clearance = cl, bypass = 1 - cl,
                  best_round = br, best_match = bm, best_channel = bc)
}

# Per-round bypass measure: the average bypass likelihood of that round's posts.
score_bypass <- function(df) {
  b <- bypass_clearance(df)
  if (nrow(b) == 0) {
    return(tibble::tibble(round_idx = integer(0), posts = integer(0),
                          pct = numeric(0)))
  }
  b |>
    dplyr::group_by(round_idx) |>
    dplyr::summarise(posts = dplyr::n(), pct = mean(bypass), .groups = "drop")
}

# --- Signal 3: Network change ------------------------------------------------
# Counts links that break the baseline pattern in EITHER direction: brand-new
# links that never appeared during the baseline period, plus reliable baseline
# links (active in at least half the baseline rounds) that go silent in a round -
# the "a normally-active connection suddenly drops" case, such as the Judge being
# cut off. weight_floor only filters out one-off links; it is not an abnormality.
score_network <- function(df, split, weight_floor = 1) {
  base_rounds <- sort(unique(df$round_idx[df$round_idx <= split]))
  links_in <- function(r) {
    e <- round_edges(r)
    e <- e[e$weight >= weight_floor, , drop = FALSE]
    if (nrow(e) == 0) character(0) else paste(e$from, e$to)
  }
  all_base <- unlist(lapply(base_rounds, links_in))
  base_any <- unique(all_base)
  freq     <- if (length(all_base) == 0) numeric(0)
              else table(all_base) / max(length(base_rounds), 1)
  reliable <- names(freq)[freq >= 0.5]   # links active in half the baseline rounds
  rounds <- sort(unique(df$round_idx))
  out <- lapply(rounds, function(r) {
    cur <- links_in(r)
    appeared <- length(setdiff(cur, base_any))   # brand-new connections
    dropped  <- length(setdiff(reliable, cur))   # normally-active links gone quiet
    tibble::tibble(round_idx = r, value = appeared + dropped)
  })
  dplyr::bind_rows(out)
}

# --- Signal 4: Topic pressure ------------------------------------------------
# Resolve a topic name and/or free-text search into a single regex pattern, the
# same way the Evidence tab does. Search wins over the dropdown when both exist.
resolve_topic_pattern <- function(topics = NULL, search = NULL) {
  # Free-text search wins. Comma- or semicolon-separate to track several words.
  if (!is.null(search) && nzchar(trimws(search %||% ""))) {
    terms <- trimws(strsplit(search, "[,;]")[[1]])
    terms <- terms[nzchar(terms)]
    if (length(terms) > 0) {
      lbl <- if (length(terms) == 1) paste0("“", terms, "”")
             else paste0("search: ", paste(terms, collapse = ", "))
      return(list(label = lbl, pattern = paste(tolower(terms), collapse = "|")))
    }
  }
  # Chosen topics combine with OR.
  topics <- topics[topics %in% topic_patterns_default$topic]
  if (length(topics) > 0) {
    pats <- topic_patterns_default$pattern[match(topics, topic_patterns_default$topic)]
    return(list(label = paste(topics, collapse = ", "),
                pattern = paste(pats, collapse = "|")))
  }
  # Nothing chosen and nothing searched -> track every topic.
  list(label = "all topics",
       pattern = paste(topic_patterns_default$pattern, collapse = "|"))
}

# Topic pressure per round, measured as a spike above the baseline-period rate.
# value = raw mentions that round. score = how far the round exceeds spike times
# the baseline-period average, so only genuine increases shade the lane. If the topic
# never appeared in baseline, any later appearance counts in full.
score_topic <- function(df, split, pattern = NULL, spike = 2) {
  perround <- if (is.null(pattern) && "embargo_keyword_count" %in% names(df)) {
    df |>
      dplyr::group_by(round_idx) |>
      dplyr::summarise(value = sum(embargo_keyword_count, na.rm = TRUE), .groups = "drop")
  } else {
    pat <- pattern %||% topic_patterns_default$pattern[topic_patterns_default$topic == "Compliance"]
    df |>
      dplyr::mutate(hit = stringr::str_detect(tolower(dplyr::coalesce(content, "")),
                                              tolower(pat))) |>
      dplyr::group_by(round_idx) |>
      dplyr::summarise(value = sum(hit), .groups = "drop")
  }
  rds <- sort(unique(df$round_idx))
  perround <- tibble::tibble(round_idx = rds) |>
    dplyr::left_join(perround, by = "round_idx") |>
    dplyr::mutate(value = tidyr::replace_na(value, 0))
  base_rate <- mean(perround$value[perround$round_idx <= split])
  if (is.na(base_rate)) base_rate <- 0
  perround |> dplyr::mutate(score = pmax(0, value - spike * base_rate))
}

# --- The status matrix --------------------------------------------------------
# One row per round per signal. `value` is the number shown in the cell; `score`
# is the severity basis (equal to value for most signals, but the spike measure
# for Topic). Severity is `score` normalised to 0-1 within each signal.
build_matrix <- function(df, split, strict, topic_pattern = NULL,
                         weight_floor = 1, spike = 2, bypass_tbl = NULL) {
  rds <- sort(unique(df$round_idx))
  if (is.null(bypass_tbl)) bypass_tbl <- score_bypass(df)
  parts <- dplyr::bind_rows(
    score_activity(df, split, strict) |>
      dplyr::transmute(round_idx, dimension = "Activity", value, score = value),
    bypass_tbl |>
      dplyr::transmute(round_idx, dimension = "Bypass", value = pct * 100,
                       score = pct * 100),
    score_network(df, split, weight_floor) |>
      dplyr::transmute(round_idx, dimension = "Network", value, score = value),
    score_topic(df, split, topic_pattern, spike) |>
      dplyr::transmute(round_idx, dimension = "Topic", value, score)
  )
  grid <- tidyr::expand_grid(round_idx = rds,
                             dimension = c("Activity", "Bypass", "Network", "Topic"))
  grid |>
    dplyr::left_join(parts, by = c("round_idx", "dimension")) |>
    dplyr::mutate(value = tidyr::replace_na(value, 0),
                  score = tidyr::replace_na(score, 0)) |>
    dplyr::group_by(dimension) |>
    dplyr::mutate(severity = if (max(score) > 0) score / max(score) else 0) |>
    dplyr::ungroup()
}

# --- Shared UI: compact analysis-highlight strip ------------------------------
# A single full-width highlight bar of live key numbers. The old right-hand
# description is dropped now that each page carries its own header and subtitle;
# desc_html is still accepted so existing call sites keep working.
reading_strip <- function(stats_output_id, desc_html = NULL) {
  card(
    class = "reading-strip",
    fill = FALSE,
    card_body(
      class = "reading-stats",
      fill = FALSE,
      htmlOutput(stats_output_id)
    )
  )
}

# Themed page-header band so the current tab is unmistakable.
page_header <- function(title, subtitle = NULL, kicker = NULL) {
  tags$div(
    class = "page-header",
    if (!is.null(kicker)) tags$div(class = "page-kicker", kicker),
    tags$div(class = "page-title", title),
    if (!is.null(subtitle)) tags$div(class = "page-subtitle", subtitle))
}

# Map a clicked matrix dimension to its destination tab and inner sub-tab.
dim_route <- function(dim) {
  switch(dim,
         Activity = list(tab = "behaviour"),
         Network  = list(tab = "connections"),
         Bypass   = list(tab = "means"),
         Topic    = list(tab = "means"),
         NULL)
}
