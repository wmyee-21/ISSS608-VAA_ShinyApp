# R/sec_bypass.R --------------------------------------------------------------
# SECTION B - The bypass.
#
# The central finding. It looks for moments an agent reached the outside world
# through a channel compliance cannot see. Every post on an unmonitored public
# channel is shown, then labelled by whether anything could account for it:
#
#   Stronger bypass: the agent left no trace on a monitored channel in the
#     post's own round or the selected number of rounds before it, so no previous
#     post accounts for reaching the outside world.
#
#   Likely explained: the agent did post on a monitored channel within that
#     window, so there was accountable discussion the Judge could see nearby.
#
# The recent-window slider sets how many rounds before the post to look back.
# The actual post text is one click away in the table.
#
# ADDED: Returns selected_round so app.R can jump to the Network tab and
# pre-set the round slider to the clicked round for cross-tab investigation.

sec_bypass_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = "25%",
      checkboxGroupInput(
        ns("agents"), "Agents to inspect",
        choiceNames  = unname(agent_labels),
        choiceValues = names(agent_labels),
        selected = names(agent_labels)),
      sliderInput(ns("window"), "Recent window to check (rounds before the post)",
                  min = 0, max = 5, value = 2, step = 1),
      helpText("This checks whether the agent left any accountable post on a ",
               "watched channel in the post's own round or the selected number ",
               "of rounds before it. If no such post accounts for the bypass it ",
               "is marked a stronger bypass; if one does, it is likely explained. ",
               "Set to 0 to check only the post's own round."),
      hr(),
      helpText("💡 Click any bar to see that round's posts below — and jump ",
               "to the Network tab to see how communication changed that round.")
    ),
    card(
      card_header("Reading this tab"),
      htmlOutput(ns("summary"))
    ),
    layout_columns(
      col_widths = c(12, 12),
      card(card_header("Posts that reached the outside world without oversight"),
           girafeOutput(ns("plot"), height = "260px"),
           card_footer("Bars are coloured by agent. Solid bars are stronger ",
                       "bypasses; faded bars had accountable discussion nearby ",
                       "and are likely explained.")),
      card(card_header("The posts themselves"),
           DTOutput(ns("tbl")),
           card_footer("Click a bar above to show only that round's posts. ",
                       "Click the same bar again to clear the filter."))
    )
  )
}

# Classify each unmonitored public post for the given messages and window.
detect_bypass <- function(df, window_n) {
  mon_rounds <- df |>
    filter(channel %in% monitored_channels) |>
    distinct(agent_id, round_idx) |>
    mutate(mon = TRUE)
  
  posts <- df |> filter(channel %in% public_unmonitored)
  
  posts |>
    rowwise() |>
    mutate(
      accounted = any(mon_rounds$agent_id == agent_id &
                        mon_rounds$round_idx >= (round_idx - window_n) &
                        mon_rounds$round_idx <= round_idx),
      strength  = if_else(!accounted, "Stronger bypass", "Likely explained")
    ) |>
    ungroup()
}

sec_bypass_server <- function(id, messages, dataRev = reactive(0)) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(dataRev(), {
      updateCheckboxGroupInput(session, "agents",
                               choiceNames  = unname(agent_labels),
                               choiceValues = names(agent_labels),
                               selected     = names(agent_labels))
    }, ignoreInit = TRUE)
    
    flagged <- reactive({
      req(input$agents)
      detect_bypass(messages(), input$window) |>
        filter(agent_id %in% input$agents)
    })
    
    output$summary <- renderUI({
      d <- flagged()
      strong <- sum(d$strength == "Stronger bypass")
      expl   <- sum(d$strength == "Likely explained")
      HTML(paste0(
        "<span style='font-size:1.7em;font-weight:700;color:#5b3650'>", strong, " stronger bypasses</span>",
        "<div style='color:#6f6673'>", expl, " likely explained &middot; look-back ",
        input$window, " round(s)</div>",
        "<p style='margin-top:.5rem;margin-bottom:0'>A bypass is a public post on a channel the ",
        "Judge cannot see. <b>Stronger</b> means the agent left no monitored-channel trace nearby, so ",
        "nothing accountable explains reaching the outside; <b>likely explained</b> means accountable ",
        "discussion sat close by.</p>"))
    })
    
    output$plot <- renderGirafe({
      df <- flagged() |>
        mutate(agent = as.character(agent_id)) |>
        count(round_idx, agent, strength, name = "n") |>
        mutate(tip = paste0(agent_labels[agent], " — Round ", round_idx,
                            ": ", n, " ", tolower(strength),
                            " — click to view in Network tab"))
      validate(need(nrow(df) > 0, "No bypass candidates for this selection."))
      ggp <- ggplot(df, aes(round_idx, n, fill = agent, alpha = strength)) +
        geom_col_interactive(aes(tooltip = tip, data_id = round_idx),
                             width = 0.8) +
        scale_fill_manual(values = agent_palette, labels = agent_labels,
                          name = NULL) +
        scale_alpha_manual(values = c("Stronger bypass" = 1,
                                      "Likely explained" = 0.45),
                           name = NULL) +
        scale_x_continuous(breaks = 1:n_rounds) +
        labs(x = "Round", y = "Posts") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
      girafe(ggobj = ggp, width_svg = 9, height_svg = 3.2,
             options = list(
               opts_selection(type = "single", only_shiny = TRUE),
               opts_hover(css = "stroke:#1A202C;stroke-width:0.8px;cursor:pointer;")))
    })
    
    output$tbl <- renderDT({
      d <- flagged()
      sel <- input$plot_selected
      if (!is.null(sel) && any(nzchar(sel))) {
        d <- d |> filter(round_idx %in% as.integer(sel))
      }
      d |>
        transmute(Round = round_idx,
                  Agent = agent_labels[as.character(agent_id)],
                  Channel = as.character(channel),
                  Assessment = strength,
                  Content = content) |>
        arrange(Round) |>
        datatable(rownames = FALSE, options = list(pageLength = 8, dom = "tip"))
    })
    
    # Return the selected round so app.R can navigate to the Network tab.
    list(selected_round = reactive(input$plot_selected))
  })
}
