source("global.R")

# --- Theme -------------------------------------------------------------------
app_theme <- bs_theme(
  version       = 5,
  bg            = "#fffdfe",
  fg            = "#312b32",
  primary       = "#7d4a67",
  secondary     = "#c4708f",
  base_font     = font_google("Nunito Sans"),
  heading_font  = font_google("Playfair Display"),
  "border-radius" = "0.6rem"
) |>
  bs_add_rules("
    /* Navbar — plum with a rose underline, matching the site */
    .navbar { background-color:#7d4a67 !important; border-bottom:3px solid #c4708f;
              box-shadow:0 2px 16px rgba(91,54,80,.18); }
    .navbar-brand { font-family:'Playfair Display',serif; font-weight:700;
                    letter-spacing:.3px; color:#ffffff !important; font-style:italic; }
    .navbar .nav-link { color:rgba(255,255,255,.84) !important; font-weight:600;
                        letter-spacing:.015em; }
    .navbar .nav-link:hover, .navbar .nav-link.active,
    .navbar .nav-link[aria-selected='true'] { color:#ffffff !important;
                        border-bottom:2px solid #e8c4d4; }

    h1,h2,h3,h4,h5 { color:#5b3650; }
    a { color:#c4708f; } a:hover { color:#5b3650; }

    /* Kicker — small investigative label */
    .kicker { font-family:'Nunito Sans',sans-serif; text-transform:uppercase;
              letter-spacing:.18em; font-size:.7rem; font-weight:700;
              color:#c4708f; margin-bottom:.4rem; }

    /* Cards */
    .card { border:1px solid #e8c4d4; border-radius:.9rem;
            box-shadow:0 6px 22px rgba(123,74,103,.08); }
    .card-header { background:#f6e8f0; color:#5b3650; font-weight:700;
                   font-family:'Playfair Display',serif; border-bottom:1px solid #e8c4d4; }
    .card-footer { background:#fffdfe; color:#6f6673; font-size:.85em; }

    /* Compact analysis-highlight strip */
    .reading-strip { background:linear-gradient(180deg,#fdf6f9,#fffdfe);
                     border-left:4px solid #c4708f; }
    .reading-strip .card-body { padding:.5rem .85rem; }
    .stat-num { font-size:1.8em; font-weight:800; color:#5b3650;
                font-family:'Playfair Display',serif; }
    .stat-sub { color:#6f6673; font-size:.9rem; }

    /* Inner sub-tabs */
    .nav-tabs .nav-link { color:#7d4a67; font-weight:600; }
    .nav-tabs .nav-link.active { color:#5b3650; border-bottom:2px solid #c4708f; }

    /* Sidebar + inputs */
    .bslib-sidebar-layout > .sidebar { background:#fdf6f9; border-right:1px solid #e8c4d4;
                                       padding-top:.6rem; }
    .bslib-sidebar-layout .sidebar-title, .bslib-sidebar-layout label { color:#5b3650; }
    .irs--shiny .irs-bar { background:#7d4a67; border-top-color:#7d4a67; border-bottom-color:#7d4a67; }
    .irs--shiny .irs-handle > i:first-child { background:#5b3650; }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:#7d4a67; }
    .form-check-input:checked { background-color:#7d4a67; border-color:#7d4a67; }
    .btn-primary { background-color:#7d4a67; border-color:#7d4a67; }
    .btn-primary:hover { background-color:#5b3650; border-color:#5b3650; }
    table.dataTable thead th { background:#f6e8f0; color:#5b3650; border-bottom:1px solid #e8c4d4; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background:#7d4a67 !important; color:#fff !important; border-radius:.3rem; }

    /* Footer credit */
    .app-footer { text-align:center; padding:.5rem 1rem; color:#6f6673;
                  font-size:.82rem; background:#fdf6f9; border-top:1px solid #e8c4d4;
                  font-family:'Nunito Sans',sans-serif; }
    .app-footer b { color:#5b3650; font-family:'Playfair Display',serif; }

    /* Compact layout — keep the page from running long */
    .card { margin-bottom:.55rem; }
    .card-body { padding:.55rem .85rem; }
    .shiny-input-container { margin-bottom:.45rem; }
    hr { margin:.45rem 0; opacity:.4; }
    .help-block { font-size:.76rem; line-height:1.25; margin-top:.1rem; color:#8a7e88; }
    .kicker { margin:.1rem 0 .25rem; }
    .form-label, label { margin-bottom:.15rem; }

    /* Page header — a dossier-style title set on the page itself rather than a
       filled band, so it does not clash with the plum navbar above it. */
    .page-header { background:transparent; border:0; box-shadow:none;
      padding:.1rem 0 .45rem; margin:.15rem 0 .8rem;
      border-bottom:1px solid #e8c4d4; position:relative; }
    .page-header::after { content:''; position:absolute; left:0; bottom:-1px;
      width:90px; height:3px; background:#c4708f; border-radius:2px; }
    .page-kicker { text-transform:uppercase; letter-spacing:.22em; font-size:.72rem;
      font-weight:700; color:#c4708f; margin-bottom:.12rem; }
    .page-title { font-family:'Playfair Display',serif; font-weight:700;
      font-size:1.95rem; line-height:1.05; color:#5b3650; }
    .page-title::first-letter { font-size:1.4em; color:#7d4a67; }
    .page-subtitle { color:#6f6673; font-size:.9rem; font-style:italic;
      margin-top:.2rem; }
  ")

# --- UI ----------------------------------------------------------------------
ui <- page_navbar(
  id    = "main_nav",
  title = "A Study in Breach",
  window_title = "A Study in Breach — CSM TraceForce",
  theme = app_theme,
  fillable = FALSE,
  nav_panel("The Case Board", value = "board",
            page_header("The Case Board", kicker = "Overview",
                        subtitle = "Every round scored on four signals. Click a hot cell to investigate."),
            sec_caseboard_ui("board")),
  nav_panel("Behaviour", value = "behaviour",
            page_header("Behaviour", kicker = "Channel drift",
                        subtitle = "Where an agent's channel mix departs from its baseline, up or down."),
            sec_behaviour_ui("behaviour")),
  nav_panel("Connections", value = "connections",
            page_header("Connections", kicker = "Network",
                        subtitle = "Who is talking to whom, and how the network changes between rounds."),
            sec_connections_ui("connections")),
  nav_panel("Evidence", value = "means",
            page_header("Evidence", kicker = "What was said",
                        subtitle = "Read the messages, with the bypass lens for posts that evaded oversight."),
            sec_means_ui("means")),
  nav_panel("Case Intake", value = "intake",
            page_header("Case Intake", kicker = "Data",
                        subtitle = "Load a dataset to analyse across every tab."),
            sec_upload_ui("upload")),
  nav_spacer(),
  nav_item(tags$span(style = "color:#fbeef4;font-size:0.85em;",
                     icon("table"), " Dataset: ",
                     tags$b(textOutput("dataset_name", inline = TRUE)))),
  footer = tags$footer(class = "app-footer",
    HTML("<b>CSM TraceForce</b> &middot; Cari-On, Be Serene, Mark it Done &middot; A Study in Breach"))
)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(rev = 0L, name = "messages_clean.csv")

  # Shared analysis settings — the single source of truth.
  settings <- reactiveValues(
    baseline_split = default_split(),
    strict         = 1,            # Unusual-channel cutoff (%) — defaults to lowest
    topic          = "Merger",     # default topic to track (the embargoed secret)
    search         = "",
    topic_spike    = 1,            # Topic spike size (x baseline) — lowest
    net_floor      = 1,            # Minimum messages per link — lowest
    focus_round    = NA_integer_,
    focus_dim      = NA_character_,
    drill          = 0L,
    focus_agents   = NULL,         # agents to read in Evidence (from Connections)
    go_evidence    = 0L,           # nonce: jump to the Evidence tab
    go_connections = 0L            # nonce: jump to Connections (from Behaviour)
  )

  # Data swap from the Case Intake tab.
  bundle <- sec_upload_server("upload")
  observeEvent(bundle(), {
    req(bundle())
    apply_bundle(bundle())
    if (!is.null(bundle()$file_name)) rv$name <- bundle()$file_name
    rv$rev <- rv$rev + 1L
  })
  dataRev <- reactive(rv$rev)
  output$dataset_name <- renderText(rv$name)

  # The Case Board owns the settings and emits drill requests.
  board_click <- sec_caseboard_server("board", settings, dataRev = dataRev)

  observeEvent(board_click(), {
    cl <- board_click(); req(cl)
    settings$focus_round <- cl$round
    settings$focus_dim   <- cl$dim
    settings$drill       <- settings$drill + 1L
    route <- dim_route(cl$dim)
    if (!is.null(route)) nav_select("main_nav", route$tab, session = session)
  })

  # Connections -> Evidence: a "Read these messages" request routes to Evidence.
  observeEvent(settings$go_evidence, {
    nav_select("main_nav", "means", session = session)
  }, ignoreInit = TRUE)

  # Behaviour -> Connections: a clicked agent routes to the Connections tab.
  observeEvent(settings$go_connections, {
    nav_select("main_nav", "connections", session = session)
  }, ignoreInit = TRUE)

  # Deep-dive tabs read the shared settings and respond to drills.
  sec_behaviour_server("behaviour", settings, dataRev = dataRev)
  sec_connections_server("connections", settings, dataRev = dataRev)
  sec_means_server("means", settings, dataRev = dataRev)
}

shinyApp(ui, server)
