# app_backup.R ----------------------------------------------------------------
# BACKUP of the original four-section app (Data, Activity & abnormality, The
# bypass, Network, Topics & evidence). The live app is now app.R ("A Study in
# Breach"). Keep this so the team can revert if needed; it is not deployed.
#
# A single-file app does not auto-source global.R, so we source it here. global.R
# loads packages (including bslib), reads the data, defines the lookups, and
# sources every R/sec_*.R section file.
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
                    letter-spacing:.3px; color:#ffffff !important; }
    .navbar .nav-link { color:rgba(255,255,255,.84) !important; font-weight:600;
                        letter-spacing:.015em; }
    .navbar .nav-link:hover, .navbar .nav-link.active,
    .navbar .nav-link[aria-selected='true'] { color:#ffffff !important;
                        border-bottom:2px solid #e8c4d4; }

    /* Headings and links */
    h1,h2,h3,h4,h5 { color:#5b3650; }
    a { color:#c4708f; }
    a:hover { color:#5b3650; }

    /* Cards */
    .card { border:1px solid #e8c4d4; border-radius:.9rem;
            box-shadow:0 6px 22px rgba(123,74,103,.08); }
    .card-header { background:#f6e8f0; color:#5b3650; font-weight:700;
                   font-family:'Playfair Display',serif; border-bottom:1px solid #e8c4d4; }
    .card-footer { background:#fffdfe; color:#6f6673; font-size:.85em; }

    /* Sidebar */
    .bslib-sidebar-layout > .sidebar { background:#fdf6f9; border-right:1px solid #e8c4d4; }
    .bslib-sidebar-layout .sidebar-title, .bslib-sidebar-layout label { color:#5b3650; }

    /* Inputs: sliders and checkboxes in the project palette */
    .irs--shiny .irs-bar { background:#7d4a67; border-top-color:#7d4a67;
                           border-bottom-color:#7d4a67; }
    .irs--shiny .irs-handle > i:first-child { background:#5b3650; }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:#7d4a67; }
    .irs--shiny .irs-min, .irs--shiny .irs-max { background:#f6e8f0; color:#5b3650; }
    .form-check-input:checked { background-color:#7d4a67; border-color:#7d4a67; }
    .form-check-input:focus { border-color:#c4708f; box-shadow:0 0 0 .2rem rgba(196,112,143,.25); }

    /* Buttons */
    .btn-primary { background-color:#7d4a67; border-color:#7d4a67; }
    .btn-primary:hover { background-color:#5b3650; border-color:#5b3650; }

    /* Evidence tables (DT) */
    table.dataTable thead th { background:#f6e8f0; color:#5b3650; border-bottom:1px solid #e8c4d4; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background:#7d4a67 !important; color:#fff !important; border-radius:.3rem; }
  ")

# id = "main_nav" is required so nav_select() can switch tabs programmatically.
ui <- page_navbar(
  id    = "main_nav",
  title = "TenantThread Breach Explorer",
  theme = app_theme,
  fillable = FALSE,
  nav_panel("Data",                   sec_upload_ui("upload")),
  nav_panel("Activity & abnormality", sec_activity_ui("activity")),
  nav_panel("The bypass",             sec_bypass_ui("bypass")),
  nav_panel("Network",                sec_network_ui("network")),
  nav_panel("Topics & evidence",      sec_topics_ui("topics")),
  nav_spacer(),
  nav_item(tags$span(style = "color:#fbeef4;font-size:0.85em;",
                     icon("table"), " Dataset: ",
                     tags$b(textOutput("dataset_name", inline = TRUE))))
)

server <- function(input, output, session) {
  rv     <- reactiveValues(rev = 0L, name = "messages_clean.csv")
  bundle <- sec_upload_server("upload")
  observeEvent(bundle(), {
    req(bundle())
    apply_bundle(bundle())
    if (!is.null(bundle()$file_name)) rv$name <- bundle()$file_name
    rv$rev <- rv$rev + 1L
  })
  dataRev <- reactive(rv$rev)

  output$dataset_name <- renderText(rv$name)

  # Section A runs first and produces the shared reactives.
  act <- sec_activity_server("activity", dataRev = dataRev)

  # Bypass now returns the selected round so we can navigate to Network.
  byp <- sec_bypass_server("bypass", messages = act$messages, dataRev = dataRev)

  # When a bypass bar is clicked, jump to the Network tab.
  # The Network server will update its own round slider via jump_round.
  observeEvent(byp$selected_round(), {
    r <- byp$selected_round()
    req(!is.null(r) && any(nzchar(r)))
    nav_select("main_nav", "Network", session = session)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Pass jump_round into Network so it can update its slider too.
  sec_network_server("network", messages = act$messages, split = act$split,
                     dataRev = dataRev, jump_round = byp$selected_round)

  sec_topics_server("topics", messages = act$messages, dataRev = dataRev)
}

shinyApp(ui, server)
