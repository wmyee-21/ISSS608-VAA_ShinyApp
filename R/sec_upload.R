# R/sec_upload.R --------------------------------------------------------------
# DATA UPLOAD (scaffold).
#
# Ingest a new message log, map its columns to the canonical schema, classify
# the channels into the accountability ladder, validate, and emit ONE confirmed
# data bundle that the analysis tabs read from.
#
# SCHEMA (see "CSV Data Specification.docx"):
#   required: round_idx, agent_id, channel, recipients, content
#   optional: reacting, rationalizing, deliberating, timestamp
#   recipients are agent ids separated by ";" or the literal "ALL".
#
# This module is self-contained and returns a reactive `bundle()`. To wire it in,
# see the INTEGRATION NOTES at the bottom of this file.

REQUIRED_FIELDS <- c(round_idx = "Round / time step",
                     agent_id  = "Sender id",
                     channel   = "Channel name",
                     recipients = "Recipients (; separated, or ALL)",
                     content   = "Message text")
OPTIONAL_FIELDS <- c(reacting = "Private reaction",
                     rationalizing = "Private justification",
                     deliberating = "Private weighing")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Fuzzy-guess a source column for a canonical field by name.
guess_col <- function(field, cols) {
  pats <- c(round_idx = "round|step|turn|idx",
            agent_id  = "agent|sender|from|actor|user",
            channel   = "channel|platform|medium",
            recipients = "recipient|to|target|audience",
            content   = "content|message|text|body",
            reacting  = "react",
            rationalizing = "rational",
            deliberating  = "deliber")
  hit <- cols[stringr::str_detect(tolower(cols), pats[[field]])]
  if (length(hit)) hit[1] else cols[1]
}

# Default classification for a channel, by name. The analyst can override.
# The six known company channels are mapped explicitly (the data is assumed to be
# from the same company, possibly a different timeframe); anything unrecognised
# falls back to a name heuristic.
guess_channel_class <- function(ch) {
  known <- c(comms_huddle    = "monitored",   # internal, the Judge watches it
             one_on_one_chat = "monitored",
             side_huddle     = "internal",    # internal but unwatched
             official_post   = "public",
             personal_post   = "public",
             anonymous_post  = "public")
  k <- tolower(ch)
  if (k %in% names(known)) return(unname(known[[k]]))
  if (grepl("post|public|anon|flex|tweet|external", k)) "public"
  else if (grepl("side|shadow|back|private|dm", k))     "internal"
  else                                                  "monitored"
}

# Transform the company's standard JSON export into the canonical message schema.
# Structure: { rounds: [ { communications: [ { agent_id, channel, recipients,
# content, internal_state{reacting,rationalizing,deliberating}, ... } ] } ] }.
# Recipients arrive as role tokens (or ALL); they are resolved to full agent ids
# using a role-to-agent map learned from the file itself.
transform_company_json <- function(path) {
  doc    <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  rounds <- doc$rounds %||% list()

  role2agent <- list()
  for (rd in rounds) for (m in rd$communications %||% list()) {
    if (!is.null(m$agent_role) && !is.null(m$agent_id)) role2agent[[m$agent_role]] <- m$agent_id
  }
  recip_alias <- c(social_manager = "social_media")  # recipient token -> agent_role

  resolve <- function(tok) {
    if (identical(tok, "ALL")) return("ALL")
    role <- if (tok %in% names(recip_alias)) recip_alias[[tok]] else tok
    ag   <- role2agent[[role]]
    if (is.null(ag)) tok else ag
  }

  rows <- list()
  for (i in seq_along(rounds)) {
    for (m in rounds[[i]]$communications %||% list()) {
      recs <- m$recipients %||% list()
      rc   <- if (length(recs) == 0) "" else
                paste(vapply(recs, resolve, character(1)), collapse = ";")
      isr  <- m$internal_state %||% list()
      rows[[length(rows) + 1]] <- tibble::tibble(
        round_idx      = as.integer(i),
        agent_id       = m$agent_id           %||% NA_character_,
        channel        = m$channel            %||% NA_character_,
        recipients_csv = rc,
        content        = m$content            %||% NA_character_,
        reacting       = isr$reacting         %||% NA_character_,
        rationalizing  = isr$rationalizing    %||% NA_character_,
        deliberating   = isr$deliberating     %||% NA_character_)
    }
  }
  dplyr::bind_rows(rows)
}

sec_upload_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = "32%",
      fileInput(ns("file"), "Upload a message log (CSV or company JSON)",
                accept = c(".csv", ".json", "text/csv", "application/json")),
      downloadButton(ns("template"), "Download CSV template", class = "btn-sm"),
      hr(),
      uiOutput(ns("mapping")),
      hr(),
      uiOutput(ns("channels")),
      actionButton(ns("confirm"), "Confirm and load data", class = "btn-primary mt-2")
    ),
    card(card_header("About this app"),
         tags$p(
           "This tool surfaces hidden communication breaches in a multi-agent ",
           "message log and traces each signal back to the messages that prove it. ",
           tags$b("To start"), ", load a CSV or the company JSON export here, or use ",
           "the built-in example, then ",
           "open the ", tags$b("Activity"), " tab and set the baseline, which defines ",
           "what counts as normal versus suspect for the abnormality analysis. Work through ",
           tags$b("Activity, Bypass, Network and Topics"),
           ", and click any chart to read the underlying messages.")),
    card(card_header("How to use this tab"),
         tags$ol(
           tags$li(tags$b("Upload your data"),
                   " — a one-row-per-message CSV, or the company's standard JSON ",
                   "export (which is mapped automatically). Use the ",
                   tags$b("Download CSV template"), " if you need the CSV format."),
           tags$li(tags$b("Map your columns."),
                   " For each field the app needs (round, sender, channel, recipients, ",
                   "message), choose the matching column from your file. The dropdowns ",
                   "pre-guess from your headers, so usually you only confirm. The five ",
                   "fields marked with * are required."),
           tags$li(tags$b("Classify each channel."),
                   " The app lists every channel it found in your data. Mark each one as ",
                   tags$b("Monitored"), " if the compliance monitor can see it, ",
                   tags$b("Internal"), " if it is private but not watched, or ",
                   tags$b("Public"), " if it reaches the outside world. This is what ",
                   "decides whether a public post counts as a bypass."),
           tags$li(tags$b("Check the validation"),
                   " panel on the right. When every item is green, click ",
                   tags$b("Confirm and load data"),
                   " and the other tabs will analyse your dataset.")
         )),
    card(card_header("Validation"), htmlOutput(ns("validate"))),
    card(card_header("Preview (first rows, mapped to the canonical schema)"),
         DTOutput(ns("preview")))
  )
}

sec_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$template <- downloadHandler(
      filename = function() "messages_template.csv",
      content  = function(f) {
        readr::write_csv(tibble::tibble(
          round_idx  = c(1L, 1L, 2L),
          agent_id   = c("legal", "pr", "social_media"),
          channel    = c("comms_huddle", "one_on_one_chat", "personal_post"),
          recipients = c("ALL", "legal", "ALL"),
          content    = c("Morning, Q2 planning kickoff.",
                         "Can we align on the statement?",
                         "Big things coming!"),
          reacting = NA, rationalizing = NA, deliberating = NA), f)
      })

    # Is the uploaded file the company JSON export rather than a CSV?
    is_json <- reactive({
      req(input$file)
      tolower(tools::file_ext(input$file$name)) == "json"
    })

    raw <- reactive({
      req(input$file, !is_json())
      readr::read_csv(input$file$datapath, show_col_types = FALSE)
    })

    # --- column mapping ------------------------------------------------------
    output$mapping <- renderUI({
      # Company JSON: fields are mapped automatically, nothing to do here.
      if (!is.null(input$file) && is_json()) {
        return(tagList(
          tags$strong("Map your columns"),
          helpText("Detected the company JSON export. Its fields are mapped to the ",
                   "app's schema automatically, so there is nothing to map. Continue ",
                   "to classify the channels below.")
        ))
      }
      # Before a file is uploaded, show a greyed example so the instructions make sense.
      if (is.null(input$file)) {
        ex <- c(round_idx = "round", agent_id = "sender", channel = "channel",
                recipients = "to", content = "message")
        return(tagList(
          tags$strong("Map your columns"),
          helpText("After you upload a file, match each field the app needs to a ",
                   "column in your file. The dropdowns will list your headers. ",
                   "Example:"),
          tags$div(style = "opacity:.55;pointer-events:none",
            lapply(names(REQUIRED_FIELDS), function(f)
              selectInput(ns(paste0("demo_", f)), paste0(REQUIRED_FIELDS[[f]], "  *"),
                          choices = ex[[f]], selected = ex[[f]])))
        ))
      }
      cols <- names(raw())
      tagList(
        tags$strong("Map your columns"),
        helpText("Pick the column in your file that holds each field. The left ",
                 "label is what the app needs; the dropdown lists your headers. ",
                 "Fields with * are required."),
        lapply(names(REQUIRED_FIELDS), function(f)
          selectInput(ns(paste0("map_", f)), paste0(REQUIRED_FIELDS[[f]], "  *"),
                      choices = cols, selected = guess_col(f, cols))),
        lapply(names(OPTIONAL_FIELDS), function(f) {
          # auto-select a matching column if one exists, otherwise (none)
          g <- guess_col(f, cols)
          sel <- if (stringr::str_detect(tolower(g), substr(f, 1, 5))) g else "(none)"
          selectInput(ns(paste0("map_", f)), OPTIONAL_FIELDS[[f]],
                      choices = c("(none)", cols), selected = sel)
        })
      )
    })

    # canonical-schema tibble
    mapped <- reactive({
      req(input$file)
      # Company JSON: transform straight to the canonical schema.
      if (is_json()) return(transform_company_json(input$file$datapath))
      df <- raw(); req(input$map_round_idx, input$map_agent_id, input$map_channel,
                       input$map_recipients, input$map_content)
      out <- tibble::tibble(
        round_idx      = suppressWarnings(as.integer(df[[input$map_round_idx]])),
        agent_id       = as.character(df[[input$map_agent_id]]),
        channel        = as.character(df[[input$map_channel]]),
        recipients_csv = as.character(df[[input$map_recipients]]),
        content        = as.character(df[[input$map_content]]))
      for (f in names(OPTIONAL_FIELDS)) {
        sel <- input[[paste0("map_", f)]]
        out[[f]] <- if (!is.null(sel) && sel != "(none)") as.character(df[[sel]]) else NA_character_
      }
      out
    })

    # --- channel classification ---------------------------------------------
    output$channels <- renderUI({
      cls <- c("Monitored (watched)"   = "monitored",
               "Internal, unmonitored" = "internal",
               "Public, unmonitored"   = "public")
      # Before a file is uploaded, show a greyed example.
      if (is.null(input$file)) {
        demo <- c(comms_huddle = "monitored", side_huddle = "internal",
                  anonymous_post = "public")
        return(tagList(
          tags$strong("Classify each channel"),
          helpText("After upload, the app lists every channel it finds and you tag ",
                   "each one. Monitored means the monitor can see it, Internal is ",
                   "private but unwatched, Public reaches the outside. A bypass is a ",
                   "post on a Public or Internal channel with no Monitored trace. ",
                   "Example:"),
          tags$div(style = "opacity:.55;pointer-events:none",
            lapply(names(demo), function(c)
              selectInput(ns(paste0("demo_ch_", make.names(c))), c,
                          choices = cls, selected = demo[[c]])))
        ))
      }
      ch <- sort(unique(mapped()$channel)); req(length(ch) > 0)
      tagList(
        tags$strong("Classify each channel"),
        helpText("Choose what each channel is. Monitored means the compliance ",
                 "monitor can see it. Internal means private to the team but not ",
                 "watched. Public means it reaches the outside world. A bypass is ",
                 "a post on a Public or Internal channel with no Monitored trace."),
        lapply(ch, function(c)
          selectInput(ns(paste0("ch_", make.names(c))), c, choices = cls,
                      selected = guess_channel_class(c)))
      )
    })

    # --- validation ----------------------------------------------------------
    checks <- reactive({
      df <- mapped()
      ch <- unique(df$channel)
      classified <- all(vapply(ch, function(c)
        !is.null(input[[paste0("ch_", make.names(c))]]), logical(1)))
      any_monitored <- classified && any(vapply(ch, function(c)
        identical(input[[paste0("ch_", make.names(c))]], "monitored"), logical(1)))
      list(
        rows     = nrow(df) > 0,
        rounds   = nrow(df) > 0 && all(!is.na(df$round_idx)),
        agents   = dplyr::n_distinct(df$agent_id) >= 2,
        channels = dplyr::n_distinct(df$channel) >= 2,
        content  = mean(!is.na(df$content) & nzchar(df$content)) > 0.5,
        classified    = classified,
        any_monitored = any_monitored
      )
    })

    output$validate <- renderUI({
      ck <- checks()
      row <- function(ok, txt)
        paste0("<div style='margin:2px 0'>",
               if (ok) "<span style='color:#2F855A'>&#10003;</span>"
               else    "<span style='color:#9b2c4f'>&#10007;</span>",
               " ", txt, "</div>")
      HTML(paste0(
        row(ck$rows,     "File has rows"),
        row(ck$rounds,   "Every round is numeric"),
        row(ck$agents,   "At least two agents"),
        row(ck$channels, "At least two channels"),
        row(ck$content,  "Most rows have message text"),
        row(ck$classified,    "Every channel is classified"),
        row(ck$any_monitored, "At least one monitored channel"),
        if (all(unlist(ck)))
          "<p style='color:#2F855A;margin-top:.6rem'>Ready. Click Confirm and load.</p>"
        else
          "<p style='color:#9b2c4f;margin-top:.6rem'>Resolve the items above before loading.</p>"))
    })

    output$preview <- renderDT({
      datatable(head(mapped(), 10), rownames = FALSE,
                options = list(dom = "t", scrollX = TRUE))
    })

    # --- confirmed bundle ----------------------------------------------------
    bundle <- reactiveVal(NULL)
    observeEvent(input$confirm, {
      req(all(unlist(checks())))
      df  <- mapped()
      ch  <- sort(unique(df$channel))
      cls <- vapply(ch, function(c) input[[paste0("ch_", make.names(c))]] %||% "monitored", character(1))
      # accountability order: monitored, then internal-unmonitored, then public
      hier <- tibble::tibble(channel = ch, class = cls)
      hier <- hier[order(match(hier$class, c("monitored", "internal", "public"))), ]
      hier$rank      <- seq_len(nrow(hier))
      hier$layer     <- ifelse(hier$class == "public", "Public", "Internal")
      hier$monitored <- hier$class == "monitored"

      agents <- sort(unique(df$agent_id))
      bundle(list(
        file_name          = input$file$name,
        messages           = df,
        channel_hierarchy  = hier,
        monitored_channels = hier$channel[hier$monitored],
        public_unmonitored = hier$channel[hier$class == "public"],
        public_channels    = hier$channel[hier$layer == "Public"],
        agents             = agents,
        # uploaded recipients already use agent ids, so the recipient lookup is identity
        recipient_to_agent = setNames(agents, agents),
        agent_labels       = setNames(tools::toTitleCase(gsub("_", " ", agents)), agents),
        agent_palette      = setNames(grDevices::hcl.colors(max(length(agents), 2), "PuRd"), agents),
        n_rounds           = max(df$round_idx, na.rm = TRUE),
        baseline_default   = max(2, round(max(df$round_idx, na.rm = TRUE) * 2 / 3))
      ))
      showNotification("Data loaded. The analysis tabs now use the uploaded data.",
                       type = "message")
    })

    bundle   # return the confirmed-bundle reactive
  })
}

# INTEGRATION NOTES ------------------------------------------------------------
# Today global.R hard-codes channel_hierarchy, monitored_channels,
# public_unmonitored, agent_labels, agent_palette and n_rounds for the
# TenantThread data, and the section modules read those globals directly. To make
# the four tabs run on uploaded data, route them through the bundle instead:
#
#   1. In app.R add the tab and capture the bundle:
#        up <- sec_upload_server("upload")
#        nav_panel("Data", sec_upload_ui("upload"))
#
#   2. Gate the other tabs on `req(up())` and pass bundle fields down, e.g. give
#      each section server the bundle (or the pieces it needs) in place of the
#      globals: agent_labels, agent_palette, channel_hierarchy, monitored_channels,
#      public_unmonitored, n_rounds, and the baseline default.
#
#   3. Keep the bundled TenantThread CSV as the default so the app still opens with
#      a worked example before anything is uploaded (set bundle() to the packaged
#      data on start, then let an upload replace it).
#
# This is a scaffold: the upload, mapping, classification and validation are
# complete, but rewiring the four section modules from globals to the bundle is
# the remaining integration step.
