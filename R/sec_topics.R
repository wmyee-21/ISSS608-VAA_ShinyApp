# R/sec_topics.R --------------------------------------------------------------
# SECTION D - Topics and evidence.
#
# Explore what agents talked about, in two directions, then read the actual
# messages.
#
#   Round to topics: scroll to a round, see which agents spoke and about what.
#     Answers "what was happening at this moment."
#   Topic to occurrences: search a word or pick a topic, see every agent, channel
#     and round it appeared in. Answers "where and when did this show up, and who
#     started it." This is what surfaces the early warning.
#
# The evidence table at the bottom always reflects the current selection, and
# exposes each message's private reasoning fields (reacting, rationalizing,
# deliberating) so the analyst can judge intent from the text.

sec_topics_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = "25%",
      radioButtons(ns("dir"), "Explore by",
                   choices = c("Round, see the topics" = "round",
                               "Topic, see where it appears" = "topic"),
                   selected = "topic"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'round'", ns("dir")),
        sliderInput(ns("round"), "Round", min = 1, max = n_rounds,
                    value = n_rounds, step = 1,
                    animate = animationOptions(interval = 900))),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'topic'", ns("dir")),
        selectInput(ns("topic"), "Topic", choices = topic_patterns_default$topic),
        textInput(ns("search"), "Or search any word")),
      helpText("The table below always shows the messages behind the current ",
               "view. Open a row to read the agent's private reasoning.")
    ),
    card(
      card_header("Reading this tab"),
      htmlOutput(ns("summary"))
    ),
    layout_columns(
      col_widths = c(12, 12),
      card(card_header(textOutput(ns("title"))),
           girafeOutput(ns("plot"), height = "320px")),
      card(card_header("Messages behind this view"),
           DTOutput(ns("tbl")))
    )
  )
}

sec_topics_server <- function(id, messages) {
  moduleServer(id, function(input, output, session) {

    # Tag every message with whichever topics its content matches.
    tagged <- reactive({
      pats <- topic_patterns_default
      df <- messages() |> mutate(content_lc = tolower(coalesce(content, "")))
      map_dfr(seq_len(nrow(pats)), function(i) {
        df |>
          filter(str_detect(content_lc, pats$pattern[i])) |>
          mutate(topic = pats$topic[i])
      })
    })

    # The rows the current view is about, used by both the chart and the table.
    selected <- reactive({
      if (input$dir == "round") {
        tagged() |> filter(round_idx == input$round)
      } else if (nzchar(input$search)) {
        messages() |>
          filter(str_detect(tolower(coalesce(content, "")),
                            tolower(input$search))) |>
          mutate(topic = paste0("search: ", input$search))
      } else {
        tagged() |> filter(topic == input$topic)
      }
    })

    # Output stats box: headline numbers plus what the tab means.
    output$summary <- renderUI({
      d <- selected()
      nmsg <- nrow(d)
      nr <- dplyr::n_distinct(d$round_idx)
      nc <- dplyr::n_distinct(d$channel)
      HTML(paste0(
        "<span style='font-size:1.7em;font-weight:700;color:#5b3650'>", nmsg, " messages</span>",
        "<div style='color:#6f6673'>across ", nr, " round(s) and ", nc, " channel(s)</div>",
        "<p style='margin-top:.5rem;margin-bottom:0'>Each bar ties a topic to where and when it ",
        "appeared. Open a row in the table to read the message text alongside the agent's private ",
        "reasoning, which is how an aggregate signal becomes concrete evidence.</p>"))
    })

    output$title <- renderText({
      if (input$dir == "round")
        paste0("Topics discussed in round ", input$round)
      else if (nzchar(input$search))
        paste0("Where '", input$search, "' appears")
      else paste0("Where the ", input$topic, " topic appears")
    })

    output$plot <- renderGirafe({
      df <- selected()
      validate(need(nrow(df) > 0, "Nothing matches the current view."))

      if (input$dir == "round") {
        # which agents, broken by topic, this round
        d <- df |> count(agent_id, topic, name = "n") |>
          mutate(tip = paste0(agent_labels[as.character(agent_id)], " — ",
                              topic, ": ", n))
        ggp <- ggplot(d, aes(agent_id, n, fill = topic)) +
          geom_col_interactive(aes(tooltip = tip, data_id = topic)) +
          scale_fill_manual(values = topic_colours, name = NULL) +
          scale_x_discrete(labels = agent_labels) +
          labs(x = NULL, y = "Messages") +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 30, hjust = 1),
                legend.position = "bottom")
      } else {
        # occurrences across rounds, coloured by channel
        d <- df |> count(round_idx, channel, name = "n") |>
          mutate(tip = paste0("Round ", round_idx, ", ", channel, ": ", n))
        ggp <- ggplot(d, aes(round_idx, n, fill = channel)) +
          geom_col_interactive(aes(tooltip = tip, data_id = channel),
                               width = 0.8) +
          scale_fill_manual(values = channel_palette, name = NULL) +
          scale_x_continuous(breaks = 1:n_rounds) +
          labs(x = "Round", y = "Mentions") +
          theme_minimal(base_size = 12) +
          theme(legend.position = "bottom")
      }
      girafe(ggobj = ggp, width_svg = 9, height_svg = 3.4)
    })

    output$tbl <- renderDT({
      selected() |>
        transmute(Round = round_idx,
                  Agent = agent_labels[as.character(agent_id)],
                  Channel = as.character(channel),
                  Content = content,
                  Reacting = reacting,
                  Rationalizing = rationalizing,
                  Deliberating = deliberating) |>
        arrange(Round) |>
        datatable(rownames = FALSE,
                  options = list(pageLength = 8, dom = "tip"),
                  escape = TRUE)
    })
  })
}
