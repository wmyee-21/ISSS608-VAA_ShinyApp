# R/sec_network.R -------------------------------------------------------------
# SECTION C - Network structure.
#
# Communication seen as relationships rather than counts. Two views, each shown
# as a side-by-side comparison of two rounds chosen with one range slider:
#
#   Graph: who messaged whom (agent to agent). Good for seeing the Judge cut off.
#   Bipartite: agents on one side, channels on the other, linked when used. Good
#     for seeing drift toward the low-accountability channels as structure.
#
# The round slider has two handles. The left handle picks the earlier round and
# the right handle the later round. Both views then draw the two rounds next to
# each other, and every link is coloured by how it differs between them: green is
# new in the later round, red dropped out before it, grey is present in both.

sec_network_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = "25%",
      radioButtons(ns("view"), "View",
                   choices = c("Who talks to whom (graph)" = "graph",
                               "Agents and channels (bipartite)" = "bip"),
                   selected = "graph"),
      sliderInput(ns("rounds"), "Compare two rounds",
                  min = 1, max = n_rounds,
                  value = c(max(1, round(n_rounds / 3)), n_rounds), step = 1),
      hr(),
      htmlOutput(ns("note")),
      hr(),
      tags$strong("Channel reference"),
      tableOutput(ns("channel_ref"))
    ),
    card(
      card_header("Reading this tab"),
      htmlOutput(ns("summary"))
    ),
    card(card_header(textOutput(ns("title"))),
         # Summary line that names the biggest connectivity loser between the two
         # selected rounds. Only populated for the graph view.
         htmlOutput(ns("diff_summary")),
         girafeOutput(ns("plot"), height = "480px"))
  )
}

# Build agent-to-agent directed edges from recipients for a set of rows.
# Recipients are short names (legal), so map them to full agent IDs (legal_agent)
# via recipient_to_agent before matching, otherwise no edges connect to nodes.
agent_edges <- function(df) {
  df |>
    filter(!is.na(recipients_csv), recipients_csv != "", recipients_csv != "ALL") |>
    separate_rows(recipients_csv, sep = ";") |>
    mutate(to = unname(recipient_to_agent[recipients_csv])) |>
    filter(!is.na(to)) |>
    transmute(from = as.character(agent_id), to = to) |>
    count(from, to, name = "weight")
}

# Colours used to mark how a link changed between the two compared rounds.
change_cols <- c(New = "#2F855A", Dropped = "#9B2C2C", Stayed = "#CBD5E0")

sec_network_server <- function(id, messages, split = NULL) {
  moduleServer(id, function(input, output, session) {

    # The two chosen rounds, smaller first. NULL until the slider has rendered.
    cmp <- reactive({
      rng <- input$rounds
      req(length(rng) == 2)
      list(r1 = min(rng), r2 = max(rng))
    })

    # Output stats box: headline numbers plus what the tab means.
    output$summary <- renderUI({
      rr <- cmp(); r1 <- rr$r1; r2 <- rr$r2
      e1 <- agent_edges(messages() |> filter(round_idx == r1))
      e2 <- agent_edges(messages() |> filter(round_idx == r2))
      newl <- nrow(dplyr::anti_join(e2, e1, by = c("from", "to")))
      drop <- nrow(dplyr::anti_join(e1, e2, by = c("from", "to")))
      HTML(paste0(
        "<span style='font-size:1.7em;font-weight:700;color:#5b3650'>", newl, " new &middot; ",
        drop, " dropped links</span>",
        "<div style='color:#6f6673'>between round ", r1, " and round ", r2, "</div>",
        "<p style='margin-top:.5rem;margin-bottom:0'>Green links are new in the later round, red ",
        "dropped, grey unchanged. An agent shedding links is moving from the centre of the ",
        "conversation to its edge, often as oversight tightens.</p>"))
    })

    output$title <- renderText({
      rng <- input$rounds
      if (length(rng) != 2) return("Comparing two rounds")
      r1 <- min(rng); r2 <- max(rng)
      switch(input$view,
             graph = paste0("Who talked to whom: round ", r1, " vs round ", r2),
             bip   = paste0("Channels used: round ", r1, " vs round ", r2))
    })

    output$note <- renderUI({
      txt <- switch(input$view,
        graph = "Two graphs, one per selected round. Each arrow is one agent messaging another, and node size is how connected the agent is. Green arrows are new in the later round, red arrows dropped out before it, grey arrows are in both.",
        bip   = "Two graphs, one per selected round. A line means the agent used that channel that round. Green lines are new in the later round, red lines dropped out before it, grey lines are in both. Watch for agents reaching the red public channels at the bottom.")
      HTML(paste0("<p style='font-size:0.9em;color:#4A5568'>", txt, "</p>"))
    })

    # Channel reference table at the bottom of the sidebar. Surfaces the
    # accountability hierarchy that the bipartite view orders its channels by.
    output$channel_ref <- renderTable({
      channel_hierarchy |>
        transmute(Rank = rank, Channel = channel, Layer = layer,
                  Monitored = if_else(monitored, "Yes", "No"))
    }, striped = TRUE, spacing = "xs", width = "100%")

    # One-line summary above the graph naming the agent that lost the most links
    # between the two selected rounds. Renders nothing for the bipartite view.
    output$diff_summary <- renderUI({
      if (input$view != "graph") return(NULL)
      rr <- cmp(); r1 <- rr$r1; r2 <- rr$r2
      if (r1 == r2) return(NULL)

      e1 <- agent_edges(messages() |> filter(round_idx == r1))
      e2 <- agent_edges(messages() |> filter(round_idx == r2))
      dropped <- anti_join(e1, e2, by = c("from", "to"))
      if (nrow(dropped) == 0) {
        return(HTML(paste0("<p style='margin:6px 0;color:#4A5568'>",
                    "No links present in round ", r1,
                    " dropped out by round ", r2, ".</p>")))
      }

      per_agent <- bind_rows(
        dropped |> count(agent = from, name = "out"),
        dropped |> count(agent = to,   name = "in_")
      ) |>
        group_by(agent) |>
        summarise(out = sum(out, na.rm = TRUE),
                  in_ = sum(in_, na.rm = TRUE),
                  total = out + in_, .groups = "drop") |>
        arrange(desc(total))

      top <- per_agent |> slice_head(n = 1)
      HTML(paste0(
        "<p style='margin:6px 0 10px 0;color:#1A202C'><b>",
        agent_labels[top$agent], "</b> lost ", top$total,
        " link", if (top$total != 1) "s" else "",
        " (", top$in_, " inbound, ", top$out, " outbound) ",
        "between round ", r1, " and round ", r2,
        ", the largest drop of any agent.</p>"))
    })

    output$plot <- renderGirafe({
      rr <- cmp(); r1 <- rr$r1; r2 <- rr$r2
      validate(need(r1 != r2, "Move the two slider handles to different rounds."))
      lab1 <- paste0("Round ", r1); lab2 <- paste0("Round ", r2)
      panel_levels <- c(lab1, lab2)

      if (input$view == "graph") {
        e1 <- agent_edges(messages() |> filter(round_idx == r1))
        e2 <- agent_edges(messages() |> filter(round_idx == r2))
        validate(need(nrow(e1) + nrow(e2) > 0,
                      "No agent-to-agent messages in either selected round."))
        k1 <- paste(e1$from, e1$to); k2 <- paste(e2$from, e2$to)

        # Explicit circle layout, reused in both panels.
        ag <- names(agent_labels)
        ang <- seq(0, 2 * pi, length.out = length(ag) + 1)[seq_along(ag)]
        pos <- tibble(name = ag, x = cos(ang), y = sin(ang))

        # Per-round degree (total links touching each node) for sizing.
        deg_of <- function(e) {
          tibble(name = ag) |>
            left_join(
              bind_rows(e |> count(from, wt = weight) |> rename(name = from),
                        e |> count(to,   wt = weight) |> rename(name = to)) |>
                group_by(name) |> summarise(deg = sum(n), .groups = "drop"),
              by = "name") |>
            mutate(deg = replace_na(deg, 0))
        }
        nodes <- bind_rows(
          pos |> left_join(deg_of(e1), by = "name") |> mutate(panel = lab1),
          pos |> left_join(deg_of(e2), by = "name") |> mutate(panel = lab2))
        nodes$panel <- factor(nodes$panel, levels = panel_levels)

        build <- function(e, status_fun, lab) {
          if (nrow(e) == 0) return(NULL)
          e |>
            mutate(status = status_fun(paste(from, to)), panel = lab) |>
            left_join(pos, by = c("from" = "name")) |> rename(x1 = x, y1 = y) |>
            left_join(pos, by = c("to"   = "name")) |> rename(x2 = x, y2 = y) |>
            mutate(tip = paste0(agent_labels[from], " to ", agent_labels[to],
                                ": ", weight, " msgs (", status, ")"))
        }
        seg <- bind_rows(
          build(e1, function(k) if_else(k %in% k2, "Stayed", "Dropped"), lab1),
          build(e2, function(k) if_else(k %in% k1, "Stayed", "New"),     lab2))
        seg$panel <- factor(seg$panel, levels = panel_levels)

        ggp <- ggplot() +
          geom_segment_interactive(
            data = seg,
            aes(x = x1, y = y1, xend = x2, yend = y2, colour = status,
                linewidth = weight, tooltip = tip, data_id = from),
            alpha = 0.6,
            arrow = arrow(length = unit(2.5, "mm"), type = "closed")) +
          geom_point_interactive(
            data = nodes,
            aes(x, y, size = deg, fill = name, data_id = name,
                tooltip = paste0(agent_labels[name], " — ", deg, " links")),
            shape = 21, colour = "white", stroke = 0.4) +
          geom_text(data = nodes, aes(x * 1.15, y * 1.15, label = agent_labels[name]),
                    size = 3) +
          scale_fill_manual(values = agent_palette, guide = "none") +
          scale_colour_manual(values = change_cols, name = NULL) +
          scale_size(range = c(3, 12), guide = "none") +
          scale_linewidth(range = c(0.4, 2.2), guide = "none") +
          coord_equal(xlim = c(-1.4, 1.4), ylim = c(-1.4, 1.4)) +
          facet_wrap(~ panel) +
          theme_void() +
          theme(legend.position = "bottom",
                strip.text = element_text(size = 12, face = "bold"))
        return(girafe(ggobj = ggp, width_svg = 9, height_svg = 5))
      }

      # bipartite view
      used_of <- function(rr_idx) {
        messages() |>
          filter(round_idx == rr_idx) |>
          distinct(agent_id, channel) |>
          transmute(agent = as.character(agent_id),
                    chan  = as.character(channel))
      }
      u1 <- used_of(r1); u2 <- used_of(r2)
      validate(need(nrow(u1) + nrow(u2) > 0,
                    "No messages in either selected round."))
      p1 <- paste(u1$agent, u1$chan); p2 <- paste(u2$agent, u2$chan)

      ag_names <- names(agent_labels)
      ch_names <- channel_hierarchy$channel
      ag_pos <- tibble(agent = ag_names, ax = 0, ay = seq_along(ag_names))
      ch_pos <- tibble(chan = ch_names, cx = 1,
                       cy = seq_along(ch_names) *
                         (length(ag_names) / length(ch_names)))

      build_b <- function(u, status_fun, lab) {
        if (nrow(u) == 0) return(NULL)
        u |>
          mutate(status = status_fun(paste(agent, chan)), panel = lab) |>
          left_join(ag_pos, by = "agent") |>
          left_join(ch_pos, by = "chan") |>
          mutate(tip = paste0(agent_labels[agent], " used ", chan,
                              " (", status, ")"))
      }
      seg <- bind_rows(
        build_b(u1, function(k) if_else(k %in% p2, "Stayed", "Dropped"), lab1),
        build_b(u2, function(k) if_else(k %in% p1, "Stayed", "New"),     lab2))
      seg$panel <- factor(seg$panel, levels = panel_levels)

      ag_nodes <- bind_rows(ag_pos |> mutate(panel = lab1),
                            ag_pos |> mutate(panel = lab2))
      ch_nodes <- bind_rows(ch_pos |> mutate(panel = lab1),
                            ch_pos |> mutate(panel = lab2))
      ag_nodes$panel <- factor(ag_nodes$panel, levels = panel_levels)
      ch_nodes$panel <- factor(ch_nodes$panel, levels = panel_levels)

      ggp <- ggplot() +
        geom_segment_interactive(
          data = seg,
          aes(x = ax, y = ay, xend = cx, yend = cy, colour = status,
              tooltip = tip, data_id = agent),
          alpha = 0.7) +
        geom_point(data = ag_nodes, aes(ax, ay, fill = agent), shape = 21,
                   size = 4.5, colour = "white", stroke = 0.3) +
        geom_text(data = ag_nodes, aes(ax - 0.05, ay, label = agent_labels[agent]),
                  hjust = 1, size = 3) +
        geom_point(data = ch_nodes, aes(cx, cy), size = 4.5,
                   colour = channel_palette[ch_nodes$chan]) +
        geom_text(data = ch_nodes, aes(cx + 0.05, cy, label = chan),
                  hjust = 0, size = 3) +
        scale_fill_manual(values = agent_palette, guide = "none") +
        scale_colour_manual(values = change_cols, name = NULL) +
        xlim(-0.6, 1.6) +
        facet_wrap(~ panel) +
        theme_void() +
        theme(legend.position = "bottom",
              strip.text = element_text(size = 12, face = "bold"))
      girafe(ggobj = ggp, width_svg = 9, height_svg = 5)
    })
  })
}
