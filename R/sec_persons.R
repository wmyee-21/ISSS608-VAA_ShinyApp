# R/sec_persons.R -------------------------------------------------------------
# Two separate top-level tabs (formerly the Persons of Interest sub-tabs):
#   Behaviour   - channel drift: where an agent's channel mix departs from its
#                 baseline, up or down.
#   Connections - the agent-to-agent network, comparing two rounds, with a
#                 per-agent connections table and a link timeline.
#
# Both read the shared settings. Cross-tab jumps (Behaviour grid -> Connections,
# Connections -> Evidence) route through the settings store, the same way the
# Case Board drills do.

change_cols_p <- c(New = "#2F855A", Dropped = "#9B2C2C", Stayed = "#CBD5E0")

# ======================= BEHAVIOUR (channel drift) ===========================

sec_behaviour_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = "25%",
      sliderInput(ns("round"), "Round to inspect", min = 1, max = n_rounds,
                  value = n_rounds, step = 1,
                  animate = animationOptions(interval = 900)),
      uiOutput(ns("round_markers")),
      helpText("All agents are shown. The baseline and cutoff are set on the ",
               "Case Board.")
    ),
    reading_strip(ns("act_stats")),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Channel activity for the selected round"),
           girafeOutput(ns("abnormal"), height = "360px"),
           card_footer("Click an agent to open the Connections tab for this round.")),
      card(card_header("When drift happens, and who drifts"),
           tags$strong("Drifting channels per round"),
           girafeOutput(ns("flagged"), height = "200px"),
           hr(),
           tags$strong("Agents that drifted most"),
           htmlOutput(ns("drifters")))
    )
  )
}

sec_behaviour_server <- function(id, settings, dataRev = reactive(0)) {
  moduleServer(id, function(input, output, session) {

    msgs <- reactive({ dataRev(); messages_tbl })

    observeEvent(dataRev(), {
      updateSliderInput(session, "round", min = 1, max = n_rounds, value = n_rounds)
    }, ignoreInit = TRUE)

    # Drill in from the Case Board (Channel drift cell).
    observeEvent(settings$drill, {
      req(settings$focus_dim == "Activity")
      if (!is.null(settings$focus_round) && !is.na(settings$focus_round))
        updateSliderInput(session, "round", value = settings$focus_round)
    }, ignoreInit = TRUE)

    cells <- reactive({
      req(settings$baseline_split, settings$strict)
      drift_cells(msgs(), settings$baseline_split, settings$strict)
    })

    output$round_markers <- renderUI({
      fpr <- cells() |>
        dplyr::group_by(round_idx) |>
        dplyr::summarise(d = sum(drift), .groups = "drop")
      max_f <- max(fpr$d, 1)
      bars <- lapply(1:n_rounds, function(r) {
        f <- fpr$d[fpr$round_idx == r]; f <- if (length(f) == 0) 0 else f
        bg <- if (f == 0) "#E2E8F0" else
          sprintf("rgba(155,44,44,%.2f)", 0.25 + (f / max_f) * 0.75)
        tags$div(style = paste0("display:inline-block;width:calc(",
          round(100 / n_rounds, 2), "% - 2px);height:10px;background:", bg,
          ";border-radius:2px;margin:0 1px;"),
          title = paste0("Round ", r, ": ", f, " drifting channel",
                         if (f != 1) "s" else ""))
      })
      tags$div(
        tags$p(style = "font-size:0.72em;color:#718096;margin:6px 0 3px 0;",
               "Drift guide — hover a bar for the count:"),
        tags$div(style = "display:flex;width:100%;margin-bottom:2px;", bars))
    })

    output$abnormal <- renderGirafe({
      req(settings$baseline_split)
      r <- input$round
      sel_agents <- names(agent_labels)
      all_channels <- channel_hierarchy$channel
      dc <- cells() |> dplyr::filter(round_idx == r)
      grid <- tidyr::expand_grid(agent_id = sel_agents, channel = all_channels) |>
        dplyr::left_join(dplyr::select(dc, agent_id, channel, n, s_round, s_base, dev),
                         by = c("agent_id", "channel")) |>
        dplyr::mutate(tip = dplyr::if_else(
          is.na(dev),
          paste0(agent_labels[agent_id], " — silent this round"),
          paste0(agent_labels[agent_id], " on ", channel, ": ",
                 scales::percent(tidyr::replace_na(s_round, 0), accuracy = 1),
                 " this round vs ", scales::percent(s_base, accuracy = 1),
                 " baseline (", dplyr::if_else(dev >= 0, "+", ""),
                 scales::percent(dev, accuracy = 1), ")")))
      validate(need(any(!is.na(grid$dev)), "No messages in this round."))
      ggp <- ggplot(grid, aes(channel, agent_id, fill = dev)) +
        geom_tile_interactive(aes(tooltip = tip, data_id = agent_id),
                              colour = "#E2E8F0", linewidth = 1) +
        geom_text(aes(label = dplyr::if_else(!is.na(n) & n > 0, as.character(n), "")),
                  size = 3.5, colour = "#1A202C") +
        scale_fill_gradient2(low = "#2C5282", mid = "#F7FAFC", high = "#9B2C2C",
                             midpoint = 0, limits = c(-1, 1), na.value = "white",
                             name = "Change vs baseline", labels = scales::percent) +
        scale_x_discrete(limits = all_channels) +
        scale_y_discrete(limits = rev(sel_agents), labels = agent_labels) +
        labs(x = NULL, y = NULL,
             title = paste0("Round ", r, " channel use vs baseline")) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              panel.grid = element_blank(), legend.position = "bottom")
      girafe(ggobj = ggp, width_svg = 6.5, height_svg = 4.4,
             options = list(
               opts_selection(type = "single", only_shiny = TRUE),
               opts_hover(css = "stroke:#5b3650;stroke-width:1.5px;cursor:pointer;")))
    })

    # Click an agent in the drift grid to open the Connections tab at this round.
    observeEvent(input$abnormal_selected, {
      sel <- input$abnormal_selected
      req(length(sel) == 1, nzchar(sel))
      settings$focus_round <- input$round
      settings$go_connections <- (settings$go_connections %||% 0L) + 1L
    }, ignoreInit = TRUE)

    output$flagged <- renderGirafe({
      df <- cells() |> dplyr::filter(drift) |>
        dplyr::count(round_idx, direction, name = "n") |>
        dplyr::mutate(tip = paste0("Round ", round_idx, ": ", n, " ", direction))
      validate(need(nrow(df) > 0, "No drift at this cutoff."))
      ggp <- ggplot(df, aes(round_idx, n, fill = direction)) +
        geom_col_interactive(aes(tooltip = tip), width = 0.8) +
        geom_vline(xintercept = settings$baseline_split + 0.5, linetype = "dashed",
                   colour = "#2C5282") +
        scale_fill_manual(values = c(increase = "#9B2C2C", decrease = "#2C5282"),
                          name = NULL) +
        scale_x_continuous(breaks = seq(1, n_rounds, 2)) +
        labs(x = "Round", y = "Drifting channels") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
      girafe(ggobj = ggp, width_svg = 5, height_svg = 2.5)
    })

    output$drifters <- renderUI({
      sus <- settings$baseline_split + 1
      df <- cells() |> dplyr::filter(drift, round_idx >= sus) |>
        dplyr::count(agent_id, name = "d") |>
        dplyr::mutate(name = agent_labels[as.character(agent_id)]) |>
        dplyr::arrange(dplyr::desc(d)) |> dplyr::slice_head(n = 5)
      if (nrow(df) == 0)
        return(HTML("<p style='margin-top:4px;color:#718096'>No drift after the baseline at this cutoff.</p>"))
      rows <- paste0("<li><b>", df$name, "</b> - ", df$d, " drifting channel",
                     dplyr::if_else(df$d != 1, "s", ""), "</li>", collapse = "")
      HTML(paste0("<ol style='margin-top:6px;padding-left:20px'>", rows, "</ol>"))
    })

    output$act_stats <- renderUI({
      sus <- settings$baseline_split + 1
      df <- cells() |> dplyr::filter(drift, round_idx >= sus)
      n <- nrow(df)
      top <- df |> dplyr::count(agent_id, name = "d") |>
        dplyr::arrange(dplyr::desc(d)) |> dplyr::slice_head(n = 1)
      topname <- if (nrow(top)) agent_labels[as.character(top$agent_id)] else "none"
      HTML(paste0("<span class='stat-num'>", n, "</span>",
        "<span class='stat-sub'> channel drifts</span>",
        "<div class='stat-sub'>after round ", settings$baseline_split,
        " &middot; top drifter: <b>", topname, "</b></div>"))
    })
  })
}

# ======================= CONNECTIONS (network) ===============================

sec_connections_ui <- function(id) {
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
      tags$strong("Channel reference"),
      tableOutput(ns("channel_ref"))
    ),
    reading_strip(ns("net_stats")),
    layout_columns(
      col_widths = c(8, 4),
      card(card_header(textOutput(ns("net_title"))),
           htmlOutput(ns("diff_summary")),
           girafeOutput(ns("net_plot"), height = "470px")),
      card(card_header(textOutput(ns("net_node_title"))),
           uiOutput(ns("net_round_pick")),
           DTOutput(ns("net_node_tbl")),
           actionButton(ns("read_link"), "Read these messages in Evidence",
                        icon = icon("envelope-open-text"),
                        class = "btn-primary btn-sm"),
           card_footer("Click a node in the graph to list its connections, ",
                       "pick a row, then read those messages."))
    ),
    card(card_header("Link timeline · when each pair was connected"),
         checkboxGroupInput(ns("tl_agents"), "Show links involving",
                            choiceNames = unname(agent_labels),
                            choiceValues = names(agent_labels),
                            selected = names(agent_labels), inline = TRUE),
         girafeOutput(ns("link_timeline"), height = "560px"),
         card_footer("Each row is a directed link. A filled cell means those ",
                     "two agents were connected that round; gaps show when a ",
                     "link formed or dropped. Only links between selected agents ",
                     "are shown, so pick at least two."))
  )
}

sec_connections_server <- function(id, settings, dataRev = reactive(0)) {
  moduleServer(id, function(input, output, session) {

    msgs <- reactive({ dataRev(); messages_tbl })
    selected_agent <- reactiveVal(NULL)

    observeEvent(dataRev(), {
      updateSliderInput(session, "rounds", min = 1, max = n_rounds,
                        value = c(max(1, round(n_rounds / 3)), n_rounds))
      updateCheckboxGroupInput(session, "tl_agents",
                               choiceNames = unname(agent_labels),
                               choiceValues = names(agent_labels),
                               selected = names(agent_labels))
      selected_agent(NULL)
    }, ignoreInit = TRUE)

    # Drill in from the Case Board (Network cell): compare baseline vs that round.
    observeEvent(settings$drill, {
      req(settings$focus_dim == "Network")
      r <- settings$focus_round
      if (!is.null(r) && !is.na(r)) {
        sp <- settings$baseline_split
        lo <- if (!is.null(sp) && !is.na(sp) && r > sp) sp else max(1L, r - 1L)
        updateSliderInput(session, "rounds", value = c(min(lo, r), max(lo, r)))
      }
    }, ignoreInit = TRUE)

    # Jump from the Behaviour grid: compare the clicked round with the one before.
    observeEvent(settings$go_connections, {
      r <- settings$focus_round
      if (!is.null(r) && !is.na(r))
        updateSliderInput(session, "rounds", value = c(max(1L, r - 1L), r))
    }, ignoreInit = TRUE)

    cmp <- reactive({
      rng <- input$rounds; req(length(rng) == 2)
      list(r1 = min(rng), r2 = max(rng))
    })

    output$net_stats <- renderUI({
      rr <- cmp()
      e1 <- round_edges(rr$r1)
      e2 <- round_edges(rr$r2)
      newl <- nrow(dplyr::anti_join(e2, e1, by = c("from", "to")))
      drop <- nrow(dplyr::anti_join(e1, e2, by = c("from", "to")))
      HTML(paste0("<span class='stat-num'>", newl, "</span>",
        "<span class='stat-sub'> new &middot; ", drop, " dropped links</span>",
        "<div class='stat-sub'>round ", rr$r1, " vs round ", rr$r2, "</div>"))
    })

    output$net_title <- renderText({
      rng <- input$rounds; if (length(rng) != 2) return("Comparing two rounds")
      r1 <- min(rng); r2 <- max(rng)
      switch(input$view,
             graph = paste0("Who talked to whom: round ", r1, " vs round ", r2),
             bip = paste0("Channels used: round ", r1, " vs round ", r2))
    })

    output$channel_ref <- renderTable({
      dataRev()
      channel_hierarchy |>
        dplyr::transmute(Rank = rank, Channel = channel, Layer = layer,
                         Monitored = dplyr::if_else(monitored, "Yes", "No"))
    }, striped = TRUE, spacing = "xs", width = "100%")

    output$diff_summary <- renderUI({
      if (input$view != "graph") return(NULL)
      rr <- cmp(); if (rr$r1 == rr$r2) return(NULL)
      e1 <- round_edges(rr$r1)
      e2 <- round_edges(rr$r2)
      dropped <- dplyr::anti_join(e1, e2, by = c("from", "to"))
      if (nrow(dropped) == 0)
        return(HTML(paste0("<p style='margin:6px 0;color:#4A5568'>No links from round ",
                           rr$r1, " dropped out by round ", rr$r2, ".</p>")))
      per_agent <- dplyr::bind_rows(
        dropped |> dplyr::count(agent = from, name = "out"),
        dropped |> dplyr::count(agent = to, name = "in_")) |>
        dplyr::group_by(agent) |>
        dplyr::summarise(out = sum(out, na.rm = TRUE), in_ = sum(in_, na.rm = TRUE),
                         total = out + in_, .groups = "drop") |>
        dplyr::arrange(dplyr::desc(total))
      top <- per_agent |> dplyr::slice_head(n = 1)
      HTML(paste0("<p style='margin:6px 0 10px 0;color:#1A202C'><b>",
        agent_labels[top$agent], "</b> lost ", top$total, " link",
        if (top$total != 1) "s" else "", " (", top$in_, " inbound, ", top$out,
        " outbound) between round ", rr$r1, " and round ", rr$r2,
        ", the largest drop of any agent.</p>"))
    })

    output$net_plot <- renderGirafe({
      rr <- cmp(); r1 <- rr$r1; r2 <- rr$r2
      validate(need(r1 != r2, "Move the two slider handles to different rounds."))
      lab1 <- paste0("Round ", r1); lab2 <- paste0("Round ", r2)
      panel_levels <- c(lab1, lab2)

      if (input$view == "graph") {
        e1 <- round_edges(r1)
        e2 <- round_edges(r2)
        validate(need(nrow(e1) + nrow(e2) > 0,
                      "No agent-to-agent messages in either selected round."))
        k1 <- paste(e1$from, e1$to); k2 <- paste(e2$from, e2$to)
        ag <- names(agent_labels)
        ang <- seq(0, 2 * pi, length.out = length(ag) + 1)[seq_along(ag)]
        pos <- tibble(name = ag, x = cos(ang), y = sin(ang))

        deg_of <- function(e) {
          if (nrow(e) == 0) return(tibble(name = ag, links = 0, msgs = 0))
          inc <- dplyr::bind_rows(
            tibble(name = e$from, w = e$weight),
            tibble(name = e$to,   w = e$weight)) |>
            dplyr::group_by(name) |>
            dplyr::summarise(links = dplyr::n(), msgs = sum(w), .groups = "drop")
          tibble(name = ag) |> dplyr::left_join(inc, by = "name") |>
            dplyr::mutate(links = tidyr::replace_na(links, 0),
                          msgs  = tidyr::replace_na(msgs, 0))
        }
        nodes <- dplyr::bind_rows(
          pos |> dplyr::left_join(deg_of(e1), by = "name") |> dplyr::mutate(panel = lab1),
          pos |> dplyr::left_join(deg_of(e2), by = "name") |> dplyr::mutate(panel = lab2))
        nodes$panel <- factor(nodes$panel, levels = panel_levels)

        make_seg <- function(e, status, lab) {
          if (is.null(e) || nrow(e) == 0) return(NULL)
          e |>
            dplyr::left_join(pos, by = c("from" = "name")) |> dplyr::rename(x1 = x, y1 = y) |>
            dplyr::left_join(pos, by = c("to" = "name"))   |> dplyr::rename(x2 = x, y2 = y) |>
            dplyr::mutate(
              status = status, panel = lab,
              len = sqrt((x2 - x1)^2 + (y2 - y1)^2),
              ox  = dplyr::if_else(len > 0,  (y2 - y1) / len * 0.05, 0),
              oy  = dplyr::if_else(len > 0, -(x2 - x1) / len * 0.05, 0),
              ax = x1 + ox, ay = y1 + oy, bx = x2 + ox, by = y2 + oy,
              dl = sqrt((bx - ax)^2 + (by - ay)^2),
              ux = dplyr::if_else(dl > 0, (bx - ax) / dl, 0),
              uy = dplyr::if_else(dl > 0, (by - ay) / dl, 0),
              x1 = ax + ux * 0.17, y1 = ay + uy * 0.17,
              x2 = bx - ux * 0.27, y2 = by - uy * 0.27,
              tip = paste0(agent_labels[from], " to ", agent_labels[to], ": ",
                           weight, " messages (", tolower(status), ")"))
        }
        dropped <- e1[!(k1 %in% k2), , drop = FALSE]
        seg <- dplyr::bind_rows(
          make_seg(e1, "Connected", lab1),
          make_seg(e2, "Connected", lab2),
          make_seg(dropped, "Dropped", lab2))
        seg$panel <- factor(seg$panel, levels = panel_levels)
        seg_conn <- dplyr::filter(seg, status == "Connected")
        seg_drop <- dplyr::filter(seg, status == "Dropped")

        ggp <- ggplot() +
          geom_segment_interactive(
            data = seg_drop,
            aes(x = x1, y = y1, xend = x2, yend = y2, tooltip = tip,
                data_id = paste(from, to)),
            colour = "#C5CDD6", linewidth = 0.9, alpha = 0.6,
            arrow = arrow(length = unit(2.8, "mm"), type = "closed")) +
          geom_segment_interactive(
            data = seg_conn,
            aes(x = x1, y = y1, xend = x2, yend = y2, colour = weight,
                tooltip = tip, data_id = paste(from, to)),
            linewidth = 1, alpha = 0.9,
            arrow = arrow(length = unit(2.8, "mm"), type = "closed")) +
          geom_point_interactive(
            data = nodes,
            aes(x, y, size = links, fill = name, data_id = name,
                tooltip = paste0(agent_labels[name], " — ", links, " link",
                                 dplyr::if_else(links == 1, "", "s"), " · ",
                                 msgs, " messages")),
            shape = 21, colour = "white", stroke = 0.5) +
          geom_text(data = nodes, aes(x * 1.16, y * 1.16, label = agent_labels[name]),
                    size = 3) +
          scale_fill_manual(values = agent_palette, guide = "none") +
          scale_colour_gradient(low = "#e8c4d4", high = "#5b3650",
                                name = "Messages per link") +
          scale_size(range = c(3, 10), guide = "none") +
          coord_equal(xlim = c(-1.45, 1.45), ylim = c(-1.45, 1.45)) +
          facet_wrap(~ panel) +
          labs(caption = "Colour intensity is message volume. Greyed arrows dropped since the earlier round.") +
          theme_void() +
          theme(legend.position = "bottom",
                plot.caption = element_text(size = 8, colour = "#8a7e88", hjust = 0.5),
                strip.text = element_text(size = 12, face = "bold"))
        return(girafe(ggobj = ggp, width_svg = 9, height_svg = 5.2,
                      options = list(
                        opts_selection(type = "single", only_shiny = TRUE),
                        opts_hover(css = "cursor:pointer;"))))
      }

      used_of <- function(rr_idx) {
        msgs() |> dplyr::filter(round_idx == rr_idx) |>
          dplyr::distinct(agent_id, channel) |>
          dplyr::transmute(agent = as.character(agent_id), chan = as.character(channel))
      }
      u1 <- used_of(r1); u2 <- used_of(r2)
      validate(need(nrow(u1) + nrow(u2) > 0, "No messages in either selected round."))
      p1 <- paste(u1$agent, u1$chan); p2 <- paste(u2$agent, u2$chan)
      ag_names <- names(agent_labels); ch_names <- channel_hierarchy$channel
      ag_pos <- tibble(agent = ag_names, ax = 0, ay = seq_along(ag_names))
      ch_pos <- tibble(chan = ch_names, cx = 1,
                       cy = seq_along(ch_names) * (length(ag_names) / length(ch_names)))
      build_b <- function(u, status_fun, lab) {
        if (nrow(u) == 0) return(NULL)
        u |> dplyr::mutate(status = status_fun(paste(agent, chan)), panel = lab) |>
          dplyr::left_join(ag_pos, by = "agent") |>
          dplyr::left_join(ch_pos, by = "chan") |>
          dplyr::mutate(tip = paste0(agent_labels[agent], " used ", chan, " (", status, ")"))
      }
      seg <- dplyr::bind_rows(
        build_b(u1, function(k) dplyr::if_else(k %in% p2, "Stayed", "Dropped"), lab1),
        build_b(u2, function(k) dplyr::if_else(k %in% p1, "Stayed", "New"), lab2))
      seg$panel <- factor(seg$panel, levels = panel_levels)
      ag_nodes <- dplyr::bind_rows(ag_pos |> dplyr::mutate(panel = lab1),
                                   ag_pos |> dplyr::mutate(panel = lab2))
      ch_nodes <- dplyr::bind_rows(ch_pos |> dplyr::mutate(panel = lab1),
                                   ch_pos |> dplyr::mutate(panel = lab2))
      ag_nodes$panel <- factor(ag_nodes$panel, levels = panel_levels)
      ch_nodes$panel <- factor(ch_nodes$panel, levels = panel_levels)
      ggp <- ggplot() +
        geom_segment_interactive(data = seg,
          aes(x = ax, y = ay, xend = cx, yend = cy, colour = status,
              tooltip = tip, data_id = agent), alpha = 0.7) +
        geom_point(data = ag_nodes, aes(ax, ay, fill = agent), shape = 21,
                   size = 4.5, colour = "white", stroke = 0.3) +
        geom_text(data = ag_nodes, aes(ax - 0.05, ay, label = agent_labels[agent]),
                  hjust = 1, size = 3) +
        geom_point(data = ch_nodes, aes(cx, cy), size = 4.5,
                   colour = channel_palette[ch_nodes$chan]) +
        geom_text(data = ch_nodes, aes(cx + 0.05, cy, label = chan), hjust = 0, size = 3) +
        scale_fill_manual(values = agent_palette, guide = "none") +
        scale_colour_manual(values = change_cols_p, name = NULL) +
        xlim(-0.6, 1.6) + facet_wrap(~ panel) + theme_void() +
        theme(legend.position = "bottom",
              strip.text = element_text(size = 12, face = "bold"))
      girafe(ggobj = ggp, width_svg = 9, height_svg = 5)
    })

    # Click a node to inspect that agent's connections, then read the messages.
    observeEvent(input$net_plot_selected, {
      sel <- input$net_plot_selected
      req(length(sel) == 1, nzchar(sel))
      if (sel %in% names(agent_labels)) selected_agent(sel)
    }, ignoreInit = TRUE)

    output$net_round_pick <- renderUI({
      req(!is.null(selected_agent()))
      rr <- cmp()
      radioButtons(session$ns("net_round"), NULL, inline = TRUE,
                   choices = setNames(c(rr$r1, rr$r2),
                                      c(paste("Round", rr$r1), paste("Round", rr$r2))),
                   selected = rr$r2)
    })

    node_links <- reactive({
      a <- selected_agent(); req(a)
      r2 <- suppressWarnings(as.integer(input$net_round %||% cmp()$r2))
      e <- round_edges(r2)
      if (nrow(e) == 0)
        return(tibble::tibble(partner = character(0), Sent = numeric(0),
                              Received = numeric(0), Total = numeric(0)))
      e |>
        dplyr::filter(from == a | to == a) |>
        dplyr::mutate(partner = dplyr::if_else(from == a, to, from),
                      dir = dplyr::if_else(from == a, "sent", "recv")) |>
        dplyr::group_by(partner) |>
        dplyr::summarise(Sent = sum(weight[dir == "sent"]),
                         Received = sum(weight[dir == "recv"]), .groups = "drop") |>
        dplyr::mutate(Total = Sent + Received) |>
        dplyr::arrange(dplyr::desc(Total))
    })

    output$net_node_title <- renderText({
      if (is.null(selected_agent())) "Connections — click a node in the graph"
      else paste0(agent_labels[selected_agent()], "'s connections in round ",
                  input$net_round %||% cmp()$r2)
    })

    output$net_node_tbl <- renderDT({
      validate(need(!is.null(selected_agent()),
                    "Click a node in the graph to list its connections."))
      d <- node_links()
      validate(need(nrow(d) > 0, "This agent has no links in the selected round."))
      d |>
        dplyr::transmute(Partner = agent_labels[partner], Sent, Received, Total) |>
        datatable(rownames = FALSE, selection = "single",
                  options = list(pageLength = 7, dom = "t"))
    })

    observeEvent(input$read_link, {
      a <- selected_agent(); req(a)
      sel <- input$net_node_tbl_rows_selected
      d <- node_links()
      partner <- if (length(sel) == 1 && sel <= nrow(d)) d$partner[sel] else NULL
      settings$focus_agents <- if (!is.null(partner)) c(a, partner) else a
      settings$go_evidence <- (settings$go_evidence %||% 0L) + 1L
    })

    output$link_timeline <- renderGirafe({
      dataRev()
      rds <- sort(unique(messages_tbl$round_idx))
      all_e <- purrr::map_dfr(rds, function(r) {
        round_edges(r) |>
          dplyr::mutate(round_idx = r)
      })
      validate(need(nrow(all_e) > 0, "No agent-to-agent links in this dataset."))
      # Keep only links where BOTH agents are selected, so a single agent (which
      # cannot link to itself) yields nothing and a set of N shows the links
      # among those N.
      sel <- input$tl_agents
      if (!is.null(sel) && length(sel) > 0)
        all_e <- all_e |> dplyr::filter(from %in% sel & to %in% sel)
      validate(need(nrow(all_e) > 0,
                    "Select at least two connected agents to show links."))
      all_e <- all_e |>
        dplyr::mutate(pair = paste0(agent_labels[from], " → ", agent_labels[to]))
      ord <- all_e |> dplyr::group_by(pair) |>
        dplyr::summarise(tot = sum(weight), .groups = "drop") |>
        dplyr::arrange(tot) |> dplyr::pull(pair)
      lvls <- c(ord, "Total links")

      grid_rows <- all_e |>
        dplyr::transmute(round_idx,
                         row = factor(pair, levels = lvls),
                         weight,
                         did = paste(from, to),
                         tip = paste0(agent_labels[from], " to ", agent_labels[to],
                                      " · round ", round_idx, ": ", weight, " messages"))
      total_rows <- all_e |>
        dplyr::group_by(round_idx) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::transmute(round_idx,
                         row = factor("Total links", levels = lvls),
                         label = as.character(n),
                         did = paste0("total_", round_idx),
                         tip = paste0("Round ", round_idx, ": ", n, " active links"))
      rl <- round_label_tbl(messages_tbl)

      ggp <- ggplot() +
        geom_tile_interactive(data = grid_rows,
          aes(round_idx, row, fill = weight, tooltip = tip, data_id = did),
          colour = "white", linewidth = 0.6) +
        geom_tile_interactive(data = total_rows,
          aes(round_idx, row, tooltip = tip, data_id = did),
          fill = "#efe3ec", colour = "white", linewidth = 0.6) +
        geom_text(data = total_rows, aes(round_idx, row, label = label),
                  size = 2.8, colour = "#5b3650", fontface = "bold") +
        scale_fill_gradient(low = "#e8c4d4", high = "#5b3650", name = "Messages per link") +
        scale_x_continuous(breaks = rl$round_idx, limits = c(0.5, n_rounds + 0.5)) +
        scale_y_discrete(limits = lvls) +
        labs(x = "Round", y = NULL) +
        theme_minimal(base_size = 11) +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(size = 8, colour = "#6f6673"),
              axis.text.y = element_text(size = 8, colour = "#5b3650"),
              legend.position = "bottom")
      girafe(ggobj = ggp, width_svg = 12, height_svg = 7,
             options = list(opts_hover(css = "stroke:#1A202C;stroke-width:0.8px;")))
    })
  })
}
