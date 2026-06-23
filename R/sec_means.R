# R/sec_means.R ---------------------------------------------------------------
# EVIDENCE - read the chats, with a bypass lens.
#
# One page. A "Bypass only" toggle decides what the page is about:
#   on  - the main table is every chat that bypassed oversight, and the plot
#         shows bypassed posts per round. The topic filter narrows to bypassed
#         messages on that topic.
#   off - the main table is all chats with each agent's private reasoning, and
#         the plot shows messages per round. The topic filter narrows to that
#         topic.
# The topic filter, agent filter and period apply in both modes. Clicking a bar
# in the plot focuses the table on that round; the clear button resets it.

sec_means_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = "25%",
      checkboxInput(ns("bypass_only"), "Bypass only", value = TRUE),
      helpText("On shows only chats that bypassed oversight. Turn off to search ",
               "all chats."),
      hr(),
      selectInput(ns("topic"), "Topic",
                  choices = c("All topics", topic_patterns_default$topic),
                  selected = "All topics"),
      textInput(ns("search"), "Or search words (comma-separated)"),
      hr(),
      checkboxGroupInput(ns("agents"), "Agents",
                         choiceNames = unname(agent_labels),
                         choiceValues = names(agent_labels),
                         selected = names(agent_labels)),
      hr(),
      radioButtons(ns("period"), "Time period",
                   choices = c("All rounds" = "all",
                               "Before breach" = "before",
                               "Crisis day" = "crisis"),
                   selected = "all"),
      hr(),
      actionButton(ns("clear"), "Show all rounds",
                   icon = icon("rotate-left"), class = "btn-sm", width = "100%"),
      helpText("Click a bar in the plot to focus the table on that round.")
    ),
    reading_strip(ns("evi_stats")),
    card(card_header(textOutput(ns("plot_title"))),
         girafeOutput(ns("main_plot"), height = "300px"),
         card_footer(textOutput(ns("plot_foot")))),
    card(card_header(textOutput(ns("tbl_title"))),
         DTOutput(ns("main_tbl")))
  )
}

sec_means_server <- function(id, settings, dataRev = reactive(0)) {
  moduleServer(id, function(input, output, session) {

    msgs <- reactive({ dataRev(); messages_tbl })
    focus_round <- reactiveVal(NULL)

    observeEvent(dataRev(), {
      updateCheckboxGroupInput(session, "agents",
                               choiceNames = unname(agent_labels),
                               choiceValues = names(agent_labels),
                               selected = names(agent_labels))
      focus_round(NULL)
    }, ignoreInit = TRUE)

    # A topic/search filter as a single lower-case regex, or NULL for everything.
    topic_pat <- reactive({
      s <- input$search %||% ""
      if (nzchar(trimws(s))) {
        terms <- trimws(strsplit(s, "[,;]")[[1]]); terms <- terms[nzchar(terms)]
        if (length(terms) > 0) return(paste(tolower(terms), collapse = "|"))
      }
      t <- input$topic
      if (is.null(t) || t == "All topics") return(NULL)
      topic_patterns_default$pattern[topic_patterns_default$topic == t]
    })

    period_filter <- function(df) {
      df |> dplyr::filter(dplyr::case_when(
        input$period == "before" ~ round_idx < 20,
        input$period == "crisis" ~ round_idx >= 20,
        TRUE ~ TRUE))
    }
    agent_filter <- function(df) {
      if (!is.null(input$agents) && length(input$agents) > 0)
        df <- df |> dplyr::filter(agent_id %in% input$agents)
      df
    }
    topic_filter <- function(df) {
      pat <- topic_pat()
      if (!is.null(pat))
        df <- df |> dplyr::filter(stringr::str_detect(
          tolower(dplyr::coalesce(content, "")), tolower(pat)))
      df
    }

    # Bypass clearance is the heavy step and depends only on the dataset.
    bp_all <- reactive({ dataRev(); bypass_clearance(messages_tbl) })

    # The two source sets, before the round focus.
    bypass_rows <- reactive({
      bp_all() |> agent_filter() |> period_filter() |> topic_filter()
    })
    all_rows <- reactive({
      msgs() |> agent_filter() |> period_filter() |> topic_filter()
    })
    current <- reactive({
      if (isTRUE(input$bypass_only)) bypass_rows() else all_rows()
    })
    # What the table and stats show, after the clicked-round focus.
    view <- reactive({
      d <- current()
      if (!is.null(focus_round())) d <- d |> dplyr::filter(round_idx == focus_round())
      d
    })

    # Plot clicks focus a round; clear and any filter change reset it.
    observeEvent(input$main_plot_selected, {
      sel <- input$main_plot_selected
      if (!is.null(sel) && length(sel) == 1 && nzchar(sel))
        focus_round(suppressWarnings(as.integer(sel)))
      else focus_round(NULL)
    }, ignoreInit = TRUE)
    observeEvent(input$clear, focus_round(NULL))
    # Only a period change can push a focused round out of range, so only that
    # clears the focus. The other filters (lens, topic, agents) keep it. This
    # also lets a Case Board drill set the lens and the round together without
    # the programmatic input updates wiping the round it just focused.
    observeEvent(input$period, focus_round(NULL), ignoreInit = TRUE)

    # ---- Cross-tab routing ---------------------------------------------------
    # Read these messages, from the Connections tab: all chats for the two agents.
    observeEvent(settings$go_evidence, {
      fa <- settings$focus_agents
      updateCheckboxInput(session, "bypass_only", value = FALSE)
      updateSelectInput(session, "topic", selected = "All topics")
      updateTextInput(session, "search", value = "")
      updateRadioButtons(session, "period", selected = "all")
      if (!is.null(fa) && length(fa) > 0)
        updateCheckboxGroupInput(session, "agents", selected = fa)
      focus_round(NULL)
    }, ignoreInit = TRUE)

    # Drill in from the Case Board.
    observeEvent(settings$drill, {
      req(settings$focus_dim %in% c("Bypass", "Topic"))
      r <- settings$focus_round
      if (settings$focus_dim == "Bypass") {
        updateCheckboxInput(session, "bypass_only", value = TRUE)
        if (!is.null(r) && !is.na(r)) focus_round(r)
      } else {
        updateCheckboxInput(session, "bypass_only", value = FALSE)
        if (!is.null(settings$search) && nzchar(settings$search)) {
          updateTextInput(session, "search", value = settings$search)
        } else {
          updateTextInput(session, "search", value = "")
          tsel <- settings$topic
          if (length(tsel) == 1 && tsel %in% topic_patterns_default$topic)
            updateSelectInput(session, "topic", selected = tsel)
          else updateSelectInput(session, "topic", selected = "All topics")
        }
        if (!is.null(r) && !is.na(r)) focus_round(r) else focus_round(NULL)
      }
    }, ignoreInit = TRUE)

    # ---- Highlight strip -----------------------------------------------------
    output$evi_stats <- renderUI({
      d <- view()
      if (isTRUE(input$bypass_only)) {
        avg <- if (nrow(d) > 0) round(100 * mean(d$bypass)) else 0
        HTML(paste0("<span class='stat-num'>", nrow(d), "</span>",
          "<span class='stat-sub'> bypassed chats</span>",
          "<div class='stat-sub'>average bypass likelihood ", avg,
          "% &middot; across ", dplyr::n_distinct(d$round_idx), " round(s)</div>"))
      } else {
        HTML(paste0("<span class='stat-num'>", nrow(d), "</span>",
          "<span class='stat-sub'> chats</span>",
          "<div class='stat-sub'>across ", dplyr::n_distinct(d$round_idx),
          " round(s) and ", dplyr::n_distinct(d$channel), " channel(s)</div>"))
      }
    })

    # ---- Plot ----------------------------------------------------------------
    output$plot_title <- renderText({
      if (isTRUE(input$bypass_only)) "Bypassed posts by round" else "Chats by round"
    })
    output$plot_foot <- renderText({
      if (isTRUE(input$bypass_only))
        "Bar height is bypassed posts that round; redder means higher likelihood. Click a bar to focus the table."
      else "Messages that round by channel. Click a bar to focus the table."
    })

    output$main_plot <- renderGirafe({
      if (isTRUE(input$bypass_only)) {
        d <- bypass_rows() |>
          dplyr::group_by(round_idx) |>
          dplyr::summarise(n = dplyr::n(), lik = mean(bypass), .groups = "drop") |>
          dplyr::mutate(tip = paste0("Round ", round_idx, ": ", n, " posts · ",
                                     round(lik * 100), "% avg bypass likelihood"))
        validate(need(nrow(d) > 0, "No bypassed posts for this filter."))
        ggp <- ggplot(d, aes(round_idx, n, fill = lik)) +
          geom_col_interactive(aes(tooltip = tip, data_id = round_idx), width = 0.8) +
          scale_fill_gradient(low = "#f6e8f0", high = "#9B2C2C", limits = c(0, 1),
                              name = "Bypass likelihood", labels = scales::percent) +
          scale_x_continuous(breaks = 1:n_rounds, limits = c(0.5, n_rounds + 0.5)) +
          labs(x = "Round", y = "Bypassed posts") +
          theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(), legend.position = "bottom")
      } else {
        d <- all_rows() |>
          dplyr::count(round_idx, channel, name = "n") |>
          dplyr::mutate(tip = paste0("Round ", round_idx, ", ", channel, ": ", n,
                                     " · click to focus"))
        validate(need(nrow(d) > 0, "No chats for this filter."))
        ggp <- ggplot(d, aes(round_idx, n, fill = channel)) +
          geom_col_interactive(aes(tooltip = tip, data_id = round_idx), width = 0.8) +
          scale_fill_manual(values = channel_palette, name = NULL) +
          scale_x_continuous(breaks = 1:n_rounds, limits = c(0.5, n_rounds + 0.5)) +
          labs(x = "Round", y = "Messages") +
          theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(), legend.position = "bottom")
      }
      girafe(ggobj = ggp, width_svg = 9, height_svg = 3.2,
             options = list(
               opts_selection(type = "single", only_shiny = TRUE),
               opts_hover(css = "stroke:#1A202C;stroke-width:0.8px;cursor:pointer;")))
    })

    # ---- Main table ----------------------------------------------------------
    output$tbl_title <- renderText({
      r <- focus_round()
      base <- if (isTRUE(input$bypass_only)) "Bypassed chats" else "All chats"
      if (is.null(r)) base else paste0(base, " · round ", r)
    })

    output$main_tbl <- renderDT({
      d <- view()
      if (isTRUE(input$bypass_only)) {
        validate(need(nrow(d) > 0, "No bypassed chats match the current filter."))
        if (is.null(focus_round())) {
          # All rounds: one row per bypassed post, closest watched message inline.
          d |>
            dplyr::arrange(dplyr::desc(bypass)) |>
            dplyr::transmute(
              Role = "Bypassed post",
              Round = round_idx,
              Agent = agent_labels[as.character(agent_id)],
              Channel = as.character(channel),
              `Bypass likelihood` = paste0(round(bypass * 100), "%"),
              Content = content,
              `Closest watched message` = dplyr::if_else(
                is.na(best_match), "none",
                paste0("(round ", best_round, ") ", best_match))) |>
            datatable(rownames = FALSE,
                      options = list(pageLength = 10, dom = "tip"), escape = TRUE)
        } else {
          # Focused round: bypassed posts plus the watched messages that support
          # them, as their own rows, so both can be read together. The Round
          # column shows which (often earlier) rounds the support came from.
          posts <- d |>
            dplyr::arrange(dplyr::desc(bypass)) |>
            dplyr::transmute(
              Role = "Bypassed post",
              Round = round_idx,
              Agent = agent_labels[as.character(agent_id)],
              Channel = as.character(channel),
              `Bypass likelihood` = paste0(round(bypass * 100), "%"),
              Content = content)
          supp <- d |>
            dplyr::filter(!is.na(best_match)) |>
            dplyr::distinct(agent_id, best_round, best_channel, best_match) |>
            dplyr::transmute(
              Role = "Supporting watched message",
              Round = best_round,
              Agent = agent_labels[as.character(agent_id)],
              Channel = best_channel,
              `Bypass likelihood` = "",
              Content = best_match)
          dplyr::bind_rows(posts, supp) |>
            dplyr::arrange(Round, Role) |>
            datatable(rownames = FALSE,
                      options = list(pageLength = 12, dom = "tip"), escape = TRUE)
        }
      } else {
        validate(need(nrow(d) > 0, "No chats match the current filter."))
        d |>
          dplyr::arrange(round_idx) |>
          dplyr::transmute(
            Round = round_idx,
            Agent = agent_labels[as.character(agent_id)],
            Channel = as.character(channel),
            Content = content,
            Reacting = reacting,
            Rationalizing = rationalizing,
            Deliberating = deliberating) |>
          datatable(rownames = FALSE,
                    options = list(pageLength = 10, dom = "tip"), escape = TRUE)
      }
    })
  })
}
