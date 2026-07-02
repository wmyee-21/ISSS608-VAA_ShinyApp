# R/sec_caseboard.R -----------------------------------------------------------
# THE CASE BOARD - the overview and control room.
#
# Owns the global analysis settings. The baseline period is shared by three
# signals - Channel drift, Network and Topic - so it is the single lever that
# defines what "normal" means. Each setting is grouped under the signal it
# affects.
#
# Two stacked panels:
#   Timeline  - one strip per tunable signal, shown as raw per-round counts that
#               do not depend on the baseline, so they are a fair guide for
#               placing it. Click a bar to set the baseline; baseline-period
#               rounds are shaded.
#   Matrix    - four abnormality signals scored against that baseline, each cell
#               clickable to drill into the matching tab.

sec_caseboard_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = "27%",

      actionButton(ns("suggest"), "Suggest settings",
                   icon = icon("lightbulb"), class = "btn-primary btn-sm",
                   width = "100%"),
      helpText("Fills in a sensible baseline and signal settings you can adjust."),
      hr(),

      tags$div(class = "kicker", "Baseline period"),
      sliderInput(ns("split"), "Last baseline round",
                  min = 2, max = max(2, n_rounds - 1),
                  value = default_split(), step = 1),
      helpText("The early rounds treated as normal. Click a bar on the timeline ",
               "to set it."),
      hr(),

      tags$div(class = "kicker", "Channel drift"),
      sliderInput(ns("strict"), "Unusual-Channel Activity",
                  min = 1, max = 20, value = 1, step = 1, post = "%"),
      helpText("Flags a channel when an agent's use of it changes from baseline ",
               "by more than this, either up or down. Move right to flag more."),
      hr(),

      tags$div(class = "kicker", "Topic"),
      selectInput(ns("topic"), "Topics to track (blank = all)",
                  choices = topic_patterns_default$topic, selected = "Merger",
                  multiple = TRUE),
      textInput(ns("search"), "Or search words (comma-separated)"),
      helpText("Leave topics blank to track all of them. A search overrides the ",
               "dropdown; separate several words with commas."),
      sliderInput(ns("spike"), "Spike size (× baseline)",
                  min = 1, max = 5, value = 1, step = 0.5),
      helpText("Flag a round when mentions are this many times the baseline average."),
      hr(),

      tags$div(class = "kicker", "Network"),
      sliderInput(ns("floor"), "Minimum messages per link",
                  min = 1, max = 5, value = 1, step = 1),
      helpText("Ignore links carrying fewer messages than this.")
    ),

    reading_strip(ns("stats"),
      "<div class='reading-title'>The Case Board</div>
       <p>Read the timeline, place the baseline, then read the heatmap. Click any
       hot cell to investigate that round in the matching tab.</p>"),

    card(
      card_header(textOutput(ns("matrix_title"))),
      girafeOutput(ns("board"), height = "470px"),
      card_footer("Top strips set the baseline — click a bar (shaded rounds are ",
                  "the baseline period). Heatmap below — darker is more abnormal; ",
                  "click a cell to investigate that round.")
    )
  )
}

sec_caseboard_server <- function(id, settings, dataRev = reactive(0)) {
  moduleServer(id, function(input, output, session) {

    click_val <- reactiveVal(NULL)

    # Bypass clearance is content-based and a little heavier, and it does not
    # depend on any slider, so compute it only when the dataset changes.
    bypass_round <- reactive({ dataRev(); score_bypass(messages_tbl) })

    # Reset the baseline slider to a new dataset's shape on load.
    observeEvent(dataRev(), {
      updateSliderInput(session, "split", min = 2, max = max(2, n_rounds - 1),
                        value = default_split())
    }, ignoreInit = TRUE)

    # Push local controls up into the shared settings store.
    observeEvent(input$split,  settings$baseline_split <- input$split)
    observeEvent(input$strict, settings$strict <- input$strict)
    observeEvent(input$topic,  settings$topic <- input$topic, ignoreNULL = FALSE)
    observeEvent(input$search, settings$search <- input$search, ignoreNULL = FALSE)
    observeEvent(input$spike,  settings$topic_spike <- input$spike)
    observeEvent(input$floor,  settings$net_floor <- input$floor)

    # One-click recommended settings. The baseline is the genuinely hard choice,
    # so it is detected from the data; the rest are sensible starting points.
    observeEvent(input$suggest, {
      sp <- suggest_baseline(messages_tbl)
      settings$baseline_split <- sp
      settings$strict <- 5
      settings$topic_spike <- 2
      settings$net_floor <- 1
      updateSliderInput(session, "split",  value = sp)
      updateSliderInput(session, "strict", value = 5)
      updateSliderInput(session, "spike",  value = 2)
      updateSliderInput(session, "floor",  value = 1)
      showNotification(
        paste0("Baseline set to round ", sp, ", just before outside posting ",
               "begins. Cutoff 5%, spike 2x, links 1. Adjust any of these as needed."),
        type = "message", duration = 6)
    }, ignoreInit = TRUE)

    output$matrix_title <- renderText({
      tp <- resolve_topic_pattern(settings$topic, settings$search)
      paste0("Abnormality across the timeline · topic: ", tp$label)
    })

    # The signals-over-time strips and the abnormality heatmap are drawn as ONE
    # figure (output$board) so they scale together and their round axes align.

    # ---- Status matrix data --------------------------------------------------
    matrix_data <- reactive({
      dataRev()
      req(settings$baseline_split, settings$strict)
      tp <- resolve_topic_pattern(settings$topic, settings$search)
      m  <- build_matrix(messages_tbl, settings$baseline_split, settings$strict,
                         topic_pattern = tp$pattern,
                         weight_floor  = settings$net_floor %||% 1,
                         spike         = settings$topic_spike %||% 2,
                         bypass_tbl    = bypass_round())
      rl <- round_label_tbl(messages_tbl)
      m  <- m |> dplyr::left_join(rl, by = "round_idx")
      ov <- m |>
        dplyr::group_by(round_idx, lab) |>
        dplyr::summarise(severity = mean(severity), .groups = "drop") |>
        dplyr::mutate(dimension = "Overall", value = NA_real_)
      full <- dplyr::bind_rows(m, ov)
      dim_levels <- c("Topic", "Network", "Bypass", "Activity", "Overall")
      full$dimension <- factor(full$dimension, levels = dim_levels)
      full |>
        dplyr::mutate(
          disp = unname(dim_display[as.character(dimension)]),
          lab_txt = dplyr::case_when(
            dimension == "Overall" ~ "",
            dimension == "Bypass"  ~ paste0(round(value), "%"),
            TRUE                   ~ as.character(round(value))),
          did = paste(as.character(dimension), round_idx, sep = "@@"),
          tip = dplyr::if_else(
            dimension == "Overall",
            paste0("Overall · round ", round_idx, " · ", lab),
            paste0(disp, " · round ", round_idx, " · ", lab, ": ",
                   dplyr::if_else(dimension == "Bypass",
                                  paste0(round(value), "% average bypass likelihood"),
                                  paste0(round(value))))))
    })

    # ---- Combined figure: signals over time (top) + heatmap (bottom) ---------
    output$board <- renderGirafe({
      split <- settings$baseline_split %||% default_split()
      tp <- resolve_topic_pattern(settings$topic, settings$search)
      rl <- round_label_tbl(messages_tbl)
      rds <- sort(unique(messages_tbl$round_idx))

      # Top: per-round counts for each tunable signal.
      s_drift <- "Number of unusual\nchannel activities"
      s_net   <- "Number of active\nconnections"
      s_topic <- "Count of topic\nmentions"
      sig_levels <- c(s_drift, s_net, s_topic)
      drift <- score_activity(messages_tbl, split, settings$strict %||% 1) |>
        dplyr::transmute(round_idx, value, signal = s_drift)
      topic <- messages_tbl |>
        dplyr::mutate(hit = stringr::str_detect(tolower(dplyr::coalesce(content, "")),
                                                tolower(tp$pattern))) |>
        dplyr::group_by(round_idx) |>
        dplyr::summarise(value = sum(hit), .groups = "drop") |>
        dplyr::mutate(signal = s_topic)
      net <- dplyr::bind_rows(lapply(rds, function(r) {
        tibble::tibble(round_idx = r,
                       value = nrow(round_edges(r)))
      })) |> dplyr::mutate(signal = s_net)
      dt <- tidyr::expand_grid(round_idx = rds, signal = sig_levels) |>
        dplyr::left_join(dplyr::bind_rows(drift, topic, net), by = c("round_idx", "signal")) |>
        dplyr::left_join(rl, by = "round_idx") |>
        dplyr::mutate(value = tidyr::replace_na(value, 0))
      dt$signal <- factor(dt$signal, levels = sig_levels)

      gg_top <- ggplot(dt, aes(round_idx, value, fill = signal)) +
        annotate("rect", xmin = -Inf, xmax = split + 0.5, ymin = -Inf, ymax = Inf,
                 fill = "#5b3650", alpha = 0.06) +
        geom_col_interactive(
          aes(tooltip = paste0(signal, " · round ", round_idx, " · ", lab, ": ",
                               value, " · click to set baseline"),
              data_id = round_idx), width = 0.85) +
        geom_vline(xintercept = split + 0.5, linetype = "dashed", colour = "#7d4a67") +
        facet_grid(signal ~ ., scales = "free_y", switch = "y") +
        scale_fill_manual(values = setNames(c("#9B2C2C", "#c4708f", "#7d4a67"),
                                            sig_levels), guide = "none") +
        scale_x_continuous(breaks = rl$round_idx, limits = c(0.5, n_rounds + 0.5),
                           expand = expansion(0)) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.text.y = element_text(size = 7, colour = "#6f6673"),
              axis.ticks.y = element_blank(),
              panel.grid.minor = element_blank(), panel.spacing = unit(3, "pt"),
              plot.margin = margin(2, 8, 0, 8),
              strip.placement = "outside",
              strip.text.y.left = element_text(angle = 0, face = "bold", size = 8.5,
                                               colour = "#5b3650"))

      # Bottom: abnormality heatmap.
      full <- matrix_data()
      cap <- round_date_caption(messages_tbl)
      cap_txt <- if (nzchar(cap)) paste0(cap, "  ·  hover for the exact date") else ""
      gg_bot <- ggplot(full, aes(round_idx, dimension, fill = severity)) +
        geom_tile_interactive(aes(tooltip = tip, data_id = did),
                              colour = "#ffffff", linewidth = 1.2) +
        geom_text(aes(label = lab_txt), size = 2.9, colour = "#3b2336") +
        scale_fill_gradient(low = "#f6e8f0", high = "#9B2C2C",
                            limits = c(0, 1), guide = "none") +
        scale_x_continuous(breaks = rl$round_idx, limits = c(0.5, n_rounds + 0.5),
                           expand = expansion(0)) +
        scale_y_discrete(labels = dim_display) +
        labs(x = "Round", y = NULL, caption = cap_txt) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(size = 8, colour = "#6f6673"),
              axis.title.x = element_text(size = 9, colour = "#6f6673"),
              axis.text.y = element_text(face = "bold", size = 9, colour = "#5b3650"),
              axis.ticks.y = element_blank(), panel.grid = element_blank(),
              plot.margin = margin(0, 8, 2, 8),
              plot.caption = element_text(size = 7.5, colour = "#8a7e88", hjust = 0))

      combined <- gg_top / gg_bot + plot_layout(heights = c(1.15, 1.0))
      girafe(ggobj = combined, width_svg = 11, height_svg = 4.6,
             options = list(
               opts_sizing(rescale = TRUE, width = 1),
               opts_selection(type = "single", only_shiny = TRUE),
               opts_hover(css = "stroke:#5b3650;stroke-width:1px;cursor:pointer;"),
               opts_tooltip(css = "background:#5b3650;color:#fff;padding:5px 8px;border-radius:5px;font-size:12px;")))
    })

    # One click handler for the whole figure: heatmap cells carry "Dim@@round"
    # and drill into a tab; timeline bars carry the round number and set baseline.
    observeEvent(input$board_selected, {
      sel <- input$board_selected
      req(length(sel) == 1, nzchar(sel))
      if (grepl("@@", sel, fixed = TRUE)) {
        parts <- strsplit(sel, "@@", fixed = TRUE)[[1]]
        dim <- parts[1]; rnd <- suppressWarnings(as.integer(parts[2]))
        if (dim %in% c("Activity", "Bypass", "Network", "Topic") && !is.na(rnd)) {
          prev_n <- (click_val()$n %||% 0)
          click_val(list(dim = dim, round = rnd, n = prev_n + 1))
        }
      } else {
        r <- suppressWarnings(as.integer(sel))
        if (!is.na(r)) {
          r <- max(2L, min(r, n_rounds - 1L))
          settings$baseline_split <- r
          updateSliderInput(session, "split", value = r)
        }
      }
      # Clear the visual selection so the clicked cell or bar does not stay
      # highlighted and block the next click. The action has already been read.
      session$sendCustomMessage(paste0(session$ns("board"), "_set"), character(0))
    }, ignoreInit = TRUE)

    output$stats <- renderUI({
      dataRev()
      req(settings$baseline_split, settings$strict)
      tp <- resolve_topic_pattern(settings$topic, settings$search)
      m  <- build_matrix(messages_tbl, settings$baseline_split, settings$strict,
                         topic_pattern = tp$pattern,
                         weight_floor  = settings$net_floor %||% 1,
                         spike         = settings$topic_spike %||% 2,
                         bypass_tbl    = bypass_round())
      rl <- round_label_tbl(messages_tbl)
      ov <- m |>
        dplyr::group_by(round_idx) |>
        dplyr::summarise(sev = mean(severity), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(sev))
      peak <- ov$round_idx[1]
      peak_lab <- rl$lab[rl$round_idx == peak]
      drivers <- m |>
        dplyr::filter(round_idx == peak, severity >= 0.5) |>
        dplyr::arrange(dplyr::desc(severity)) |>
        dplyr::pull(dimension)
      driver_txt <- if (length(drivers) > 0)
        paste(unname(dim_display[as.character(drivers)]), collapse = ", ")
      else "no single signal dominates"
      HTML(paste0(
        "<span class='stat-num'>Round ", peak, "</span>",
        "<span class='stat-sub'> · ", peak_lab, "</span>",
        "<div class='stat-sub'>Most abnormal round, led by <b>", driver_txt, "</b></div>",
        "<div class='stat-sub'>Baseline ends at round ", settings$baseline_split, "</div>"))
    })

    click_val
  })
}
