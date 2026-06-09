# app.R -----------------------------------------------------------------------
# Entry point. Open in RStudio and click "Run App", or run shiny::runApp() with
# the working directory set to this folder.
#
# A single-file app does not auto-source global.R, so we source it here. global.R
# loads packages (including bslib), reads the data, defines the lookups, and
# sources every R/sec_*.R section file.
source("global.R")
#
# Wiring: Section A returns a reactive list (the chosen calm-period split and the
# re-labelled messages). That list is passed into every other section, so moving
# the calm-period slider on the first tab recomputes the others together.

# --- Theme -------------------------------------------------------------------
# Matches the project website (theme.scss): plum primary, rose accents, blush
# surfaces, Playfair Display headings over a Nunito Sans body. bs_add_rules layers
# the navbar, card, sidebar and input styling that mirrors the site.
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

ui <- page_navbar(
  title = "TenantThread Breach Explorer",
  theme = app_theme,
  fillable = FALSE,
  nav_panel("Activity & abnormality", sec_activity_ui("activity")),
  nav_panel("The bypass",             sec_bypass_ui("bypass")),
  nav_panel("Network",                sec_network_ui("network")),
  nav_panel("Topics & evidence",      sec_topics_ui("topics")),
  nav_spacer(),
  nav_item(tags$span(style = "color:#fbeef4;font-size:0.85em;",
                     "VAST 2026 MC1 · set the calm period on the Activity tab"))
)

server <- function(input, output, session) {
  # Section A runs first and produces the shared reactives.
  act <- sec_activity_server("activity")

  # Pass them into the other sections.
  sec_bypass_server ("bypass",  messages = act$messages)
  sec_network_server("network", messages = act$messages, split = act$split)
  sec_topics_server ("topics",  messages = act$messages)
}

shinyApp(ui, server)
