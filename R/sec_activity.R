# R/sec_activity.R ------------------------------------------------------------
# SECTION A - Activity and abnormality.
#
# One workflow: decide what counts as the calm period, then look at how each
# agent spreads its messages across channels, with the parts that are out of
# character highlighted, and tune how strict the flagging is. This merges the old
# baseline, channel, and deviance tabs because they were always one task.
#
# What the analyst learns:
#   - what normal looks like, and whether the breach finding survives a different
#     choice of calm period
#   - which agents start using channels they never used before (the abnormal bit)
#   - which exact messages are out of character, at the strictness they choose

sec_activity_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = "25%",
      sliderInput(ns("split"), "End of the baseline period (round)",
                  min = 5, max = n_rounds - 1, value = 20, step = 1),
      helpText("Rounds up to here are the baseline period. Later rounds are the ",
               "period under suspicion. Everything else compares against this."),
      hr(),
      checkboxGroupInput(
        ns("agents"), "Agents to show",
        choiceNames  = unname(agent_labels),
        choiceValues = names(agent_labels),
        selected = names(agent_labels)
      ),
      sliderInput(ns("round"), "Round to inspect", min = 1, max = n_rounds,
                  value = n_rounds, step = 1, animate =
                    animationOptions(interval = 900)),
      hr(),
      sliderInput(ns("strict"), "How unusual before we flag a message",
                  min = 1, max = 20, value = 5, step = 1, post = "%"),
      helpText("A message is flagged when the agent used that channel in fewer ",
               "than this share of their baseline-period messages. Lower = stricter.")
    ),
    card(
      card_header("Reading this tab"),
      htmlOutput(ns("summary"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Channel activity for the selected round"),
           girafeOutput(ns("abnormal"), height = "360px"),
           card_footer("Brighter red means the agent rarely or never used that ",
                       "channel during the baseline period. Scroll or play the round ",
                       "slider to watch new channels light up.")),
      card(card_header("How robust is the finding"),
           girafeOutput(ns("flagged"), height = "200px"),
           htmlOutput(ns("robust")),
           hr(),
           tags$strong("Agents that drifted most in the suspect period"),
           htmlOutput(ns("drifters")))
    )
  )
}

sec_activity_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Re-label period from the slider. This is the shared reactive other
    # sections also receive.
    msgs <- reactive({
      split <- input$split
      messages_tbl |>
        mutate(period = factor(
          if_else(round_idx <= split, "Calm period", "Suspect period"),
          levels = c("Calm period", "Suspect period")))
    })

    # Abnormality grid for the chosen round: each agent-channel cell shaded by
    # how unusual that channel is for that agent, judged on the calm period.
    output$abnormal <- renderGirafe({
      req(input$agents)

      # Fixed axes: every selected agent and every channel is always present, in a
      # stable order, so the grid never reshapes between rounds. Only the cell
      # colour and count change, which makes the round-to-round change easy to
      # track. Empty cells (no message that round) stay blank.
      sel_agents   <- intersect(names(agent_labels), input$agents)
      all_channels <- channel_hierarchy$channel

      base_share <- agent_channel_baseline(msgs(), input$split) |>
        transmute(agent_id = as.character(agent_id),
                  channel  = as.character(channel), share)

      counts <- msgs() |>
        filter(round_idx == input$round, agent_id %in% sel_agents) |>
        count(agent_id, channel, name = "msgs") |>
        mutate(agent_id = as.character(agent_id),
               channel  = as.character(channel))

      this_round <- tidyr::expand_grid(agent_id = sel_agents,
                                       channel  = all_channels) |>
        left_join(counts,     by = c("agent_id", "channel")) |>
        left_join(base_share, by = c("agent_id", "channel")) |>
        mutate(msgs  = replace_na(msgs, 0L),
               share = replace_na(share, 0),
               # unusualness: 1 when the agent never used this channel in calm.
               # Only colour cells with activity this round; blanks stay neutral.
               unusual  = 1 - share,
               fill_val = if_else(msgs > 0, unusual, NA_real_),
               tip = paste0(agent_labels[agent_id], " on ", channel,
                            ": ", msgs, " msgs this round. ",
                            scales::percent(share, accuracy = 1),
                            " of their baseline-period messages were here."))

      ggp <- ggplot(this_round,
                    aes(x = channel, y = agent_id, fill = fill_val)) +
        geom_tile_interactive(aes(tooltip = tip, data_id = agent_id),
                              colour = "#E2E8F0", linewidth = 1) +
        geom_text(aes(label = if_else(msgs > 0, as.character(msgs), "")),
                  size = 3.5, colour = "#1A202C") +
        scale_fill_gradient(low = "#EDF2F7", high = "#9B2C2C",
                            limits = c(0, 1), na.value = "white", guide = "none") +
        scale_x_discrete(limits = all_channels) +
        scale_y_discrete(limits = rev(sel_agents), labels = agent_labels) +
        labs(x = NULL, y = NULL,
             title = paste0("Round ", input$round, " channel use")) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              panel.grid = element_blank())
      girafe(ggobj = ggp, width_svg = 6.5, height_svg = 4)
    })

    # Flagged-message timeline plus a robustness note comparing two calm periods.
    score <- function(df, last_base, strict) {
      sh <- agent_channel_baseline(df, last_base) |> select(agent_id, channel, share)
      df |>
        left_join(sh, by = c("agent_id", "channel")) |>
        mutate(share = replace_na(share, 0), flagged = share < strict / 100)
    }

    output$flagged <- renderGirafe({
      df <- score(msgs(), input$split, input$strict) |>
        group_by(round_idx) |>
        summarise(flagged = sum(flagged), .groups = "drop") |>
        mutate(tip = paste0("Round ", round_idx, ": ", flagged, " unusual"))
      ggp <- ggplot(df, aes(round_idx, flagged)) +
        geom_col_interactive(aes(tooltip = tip), fill = "#9B2C2C", width = 0.8) +
        geom_vline(xintercept = input$split + 0.5, linetype = "dashed",
                   colour = "#2C5282") +
        scale_x_continuous(breaks = seq(1, n_rounds, 2)) +
        labs(x = "Round", y = "Unusual msgs") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      girafe(ggobj = ggp, width_svg = 5, height_svg = 2.2)
    })

    output$robust <- renderUI({
      strict <- input$strict
      wide   <- score(msgs(), input$split, strict)
      narrow <- score(msgs(), max(13, input$split - 7), strict)
      sus <- input$split + 1
      w <- sum(wide$flagged   & wide$round_idx   >= sus)
      nn <- sum(narrow$flagged & narrow$round_idx >= sus)
      HTML(paste0(
        "<p style='margin-top:8px'>Unusual messages in the suspect period: <b>",
        w, "</b> using the chosen baseline, <b>", nn,
        "</b> using an earlier baseline. ",
        if (abs(w - nn) <= max(2, 0.25 * w))
          "The two are close, so the finding does not depend much on where the line is drawn."
        else
          "The two differ, so the finding is somewhat sensitive to where the line is drawn.",
        "</p>"))
    })

    # Ranks agents by how many of their suspect-period messages were flagged as
    # unusual, answering Objective 1's "which agents drifted and how far".
    output$drifters <- renderUI({
      sus <- input$split + 1
      df <- score(msgs(), input$split, input$strict) |>
        filter(round_idx >= sus) |>
        group_by(agent_id) |>
        summarise(unusual = sum(flagged), total = n(), .groups = "drop") |>
        mutate(pct = if_else(total > 0, unusual / total, 0),
               name = agent_labels[as.character(agent_id)]) |>
        arrange(desc(unusual)) |>
        slice_head(n = 5)

      if (nrow(df) == 0 || sum(df$unusual) == 0) {
        return(HTML("<p style='margin-top:4px;color:#718096'>",
                    "No drifters at the current strictness setting.</p>"))
      }

      rows <- paste0(
        "<li><b>", df$name, "</b> - ", df$unusual, " unusual messages (",
        scales::percent(df$pct, accuracy = 1), " of their suspect-period traffic)</li>",
        collapse = "")
      HTML(paste0("<ol style='margin-top:6px;padding-left:20px'>",
                  rows, "</ol>"))
    })

    # Output stats box: headline numbers plus what the tab means.
    output$summary <- renderUI({
      sus <- input$split + 1
      df  <- score(msgs(), input$split, input$strict) |> filter(round_idx >= sus)
      n   <- sum(df$flagged)
      top <- df |> group_by(agent_id) |>
        summarise(u = sum(flagged), .groups = "drop") |>
        arrange(desc(u)) |> slice_head(n = 1)
      topname <- if (nrow(top) && top$u > 0) agent_labels[as.character(top$agent_id)] else "none"
      HTML(paste0(
        "<span style='font-size:1.7em;font-weight:700;color:#5b3650'>", n, " unusual messages</span>",
        "<div style='color:#6f6673'>flagged after round ", input$split,
        " &middot; top drifter: <b>", topname, "</b></div>",
        "<p style='margin-top:.5rem;margin-bottom:0'>Unusual means an agent used a channel it ",
        "rarely touched during the calm period. A cluster of these after the baseline points to an ",
        "agent breaking its own pattern, which is the first signal worth investigating.</p>"))
    })

    # Public API for the other sections.
    list(messages = msgs, split = reactive(input$split))
  })
}
