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
      selectInput(ns("topic"), "Topics to track (one or more)",
                  choices = topic_patterns_default$topic, selected = "Merger",
                  multiple = TRUE),
      textInput(ns("search"), "Or search any word"),
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
      card_header("Choose the baseline · signals over time"),
      girafeOutput(ns("timeline"), height = "450px"),
      card_footer("Each strip is a signal that has a setting. Click any bar to ",
                  "set the baseline; shaded rounds are the baseline period.")
    ),
    card(
      card_header(textOutput(ns("matrix_title"))),
      girafeOutput(ns("matrix"), height = "330px"),
      card_footer("Darker means more abnormal, judged within each signal. ",
                  "Click a cell to investigate that round in detail.")
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

    # ---- Baseline-selection timeline -----------------------------------------
    # One strip per signal that has a setting, shown as raw per-round counts that
    # do NOT depend on the baseline, so they are a fair guide for placing it.
    output$timeline <- renderGirafe({
      dataRev()
      split <- settings$baseline_split %||% default_split()
      tp <- resolve_topic_pattern(settings$topic, settings$search)
      rl <- round_label_tbl(messages_tbl)
      rds <- sort(unique(messages_tbl$round_idx))

      s_drift <- "Unusual channel activity"
      s_topic <- paste0("Mentions of ", tp$label)
      s_net   <- "Active links"
      sig_levels <- c(s_drift, s_topic, s_net)

      drift <- score_activity(messages_tbl, split, settings$strict %||% 1) |>
        dplyr::transmute(round_idx, value, signal = s_drift)
      topic <- messages_tbl |>
        dplyr::mutate(hit = stringr::str_detect(tolower(dplyr::coalesce(content, "")),
                                                tolower(tp$pattern))) |>
        dplyr::group_by(round_idx) |>
        dplyr::summarise(value = sum(hit), .groups = "drop") |>
        dplyr::mutate(signal = s_topic)
      net <- dplyr::bind_rows(lapply(rds, function(r) {
        e <- build_agent_edges(messages_tbl |> dplyr::filter(round_idx == r))
        tibble::tibble(round_idx = r, value = nrow(e))
      })) |> dplyr::mutate(signal = s_net)

      d <- tidyr::expand_grid(round_idx = rds, signal = sig_levels) |>
        dplyr::left_join(dplyr::bind_rows(drift, topic, net),
                         by = c("round_idx", "signal")) |>
        dplyr::left_join(rl, by = "round_idx") |>
        dplyr::mutate(value = tidyr::replace_na(value, 0))
      d$signal <- factor(d$signal, levels = sig_levels)
      cap <- round_date_caption(messages_tbl)
      cap_txt <- if (nzchar(cap)) paste0(cap, "  ·  hover a bar for its date") else ""

      ggp <- ggplot(d, aes(round_idx, value, fill = signal)) +
        annotate("rect", xmin = -Inf, xmax = split + 0.5, ymin = -Inf, ymax = Inf,
                 fill = "#5b3650", alpha = 0.06) +
        geom_col_interactive(
          aes(tooltip = paste0(signal, " · round ", round_idx, " · ", lab, ": ",
                               value, " · click to set baseline"),
              data_id = round_idx), width = 0.85) +
        geom_vline(xintercept = split + 0.5, linetype = "dashed", colour = "#7d4a67") +
        facet_grid(signal ~ ., scales = "free_y", switch = "y") +
        scale_fill_manual(values = setNames(c("#9B2C2C", "#7d4a67", "#c4708f"),
                                            sig_levels), guide = "none") +
        scale_x_continuous(breaks = rl$round_idx) +
        labs(x = "Round", y = NULL, caption = cap_txt) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 8, colour = "#6f6673"),
              axis.title.x = element_text(size = 9, colour = "#6f6673"),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(3, "pt"),
              plot.margin = margin(2, 4, 2, 2),
              plot.caption = element_text(size = 7.5, colour = "#8a7e88", hjust = 0),
              strip.placement = "outside",
              strip.text.y.left = element_text(angle = 0, face = "bold", size = 8.5,
                                               colour = "#5b3650"))
      girafe(ggobj = ggp, width_svg = 11, height_svg = 6.4,
             options = list(
               opts_selection(type = "single", only_shiny = TRUE),
               opts_hover(css = "stroke:#5b3650;stroke-width:1px;cursor:pointer;")))
    })

    observeEvent(input$timeline_selected, {
      sel <- input$timeline_selected
      req(length(sel) == 1, nzchar(sel))
      r <- suppressWarnings(as.integer(sel))
      req(!is.na(r))
      r <- max(2L, min(r, n_rounds - 1L))
      settings$baseline_split <- r
      updateSliderInput(session, "split", value = r)
    }, ignoreInit = TRUE)

    # ---- Status matrix -------------------------------------------------------
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

    output$matrix <- renderGirafe({
      full <- matrix_data()
      rl <- round_label_tbl(messages_tbl)
      cap <- round_date_caption(messages_tbl)
      cap_txt <- if (nzchar(cap)) paste0(cap, "  ·  hover a cell for its date") else ""
      ggp <- ggplot(full, aes(x = round_idx, y = dimension, fill = severity)) +
        geom_tile_interactive(aes(tooltip = tip, data_id = did),
                              colour = "#ffffff", linewidth = 1.2) +
        geom_text(aes(label = lab_txt), size = 2.9, colour = "#3b2336") +
        scale_fill_gradient(low = "#f6e8f0", high = "#9B2C2C",
                            limits = c(0, 1), guide = "none") +
        scale_x_continuous(breaks = rl$round_idx) +
        scale_y_discrete(labels = dim_display) +
        labs(x = "Round", y = NULL, caption = cap_txt) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(size = 8, colour = "#6f6673"),
              axis.title.x = element_text(size = 9, colour = "#6f6673"),
              axis.text.y = element_text(face = "bold", colour = "#5b3650"),
              panel.grid = element_blank(),
              plot.caption = element_text(size = 7.5, colour = "#8a7e88", hjust = 0))
      girafe(ggobj = ggp, width_svg = 11, height_svg = 3.3,
             options = list(
               opts_selection(type = "single", only_shiny = TRUE),
               opts_hover(css = "stroke:#5b3650;stroke-width:1.5px;cursor:pointer;"),
               opts_tooltip(css = "background:#5b3650;color:#fff;padding:5px 8px;border-radius:5px;font-size:12px;")))
    })

    observeEvent(input$matrix_selected, {
      sel <- input$matrix_selected
      req(length(sel) == 1, nzchar(sel))
      parts <- strsplit(sel, "@@", fixed = TRUE)[[1]]
      dim <- parts[1]; rnd <- suppressWarnings(as.integer(parts[2]))
      if (dim %in% c("Activity", "Bypass", "Network", "Topic") && !is.na(rnd)) {
        prev_n <- (click_val()$n %||% 0)
        click_val(list(dim = dim, round = rnd, n = prev_n + 1))
      }
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
