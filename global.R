# global.R --------------------------------------------------------------------
# Runs once at app start. Loads packages, reads the cleaned data, defines the
# fixed lookups every section reuses (agent and channel colours, the channel
# accountability ladder, the topic keyword list), and precomputes the small
# tables the bypass section needs so interaction stays instant.

library(shiny)
library(bslib)        # page layout: page_navbar, card, sidebar, layout_columns
library(tidyverse)
library(lubridate)
library(DT)           # interactive evidence table
library(ggiraph)      # hover tooltips on ggplot charts
library(tidygraph)    # graph structures for the network section
library(ggraph)       # network rendering in the ggplot grammar
library(igraph)       # density and degree calculations
library(scales)

# --- Load cleaned data -------------------------------------------------------
# Same three CSVs produced in report Section 4. Types are set so factors order
# correctly on every axis and logicals stay logical.
messages_tbl <- readr::read_csv(
  "data/messages_clean.csv",
  col_types = readr::cols(
    round_idx             = readr::col_integer(),
    round_hour_dt         = readr::col_datetime(),
    timestamp_dt          = readr::col_datetime(),
    agent_id              = readr::col_factor(levels = c(
      "legal_agent","quality_agent","pr_agent","social_media_agent",
      "pr_intern_agent","intern_agent","judge_agent")),
    channel               = readr::col_factor(levels = c(
      "comms_huddle","one_on_one_chat","side_huddle",
      "official_post","personal_post","anonymous_post")),
    word_count            = readr::col_integer(),
    embargo_keyword_count = readr::col_integer(),
    recipients_n          = readr::col_integer(),
    recipients_is_all     = readr::col_logical(),
    has_internal_state    = readr::col_logical(),
    embargo_hit           = readr::col_logical(),
    .default              = readr::col_character()
  )
)

rounds_tbl <- readr::read_csv(
  "data/rounds_clean.csv",
  col_types = readr::cols(
    round_idx      = readr::col_integer(),
    round_hour_dt  = readr::col_datetime(),
    n_messages     = readr::col_integer(),
    n_participants = readr::col_integer(),
    .default       = readr::col_character()
  )
)

# --- Fixed lookups (report Section 4.3) --------------------------------------
agent_labels <- c(
  legal_agent        = "Legal",
  quality_agent      = "Platform Trust",
  pr_agent           = "PR",
  social_media_agent = "Social Media",
  pr_intern_agent    = "PR Intern",
  intern_agent       = "Intern",
  judge_agent        = "Judge"
)

agent_palette <- c(
  legal_agent        = "#2C5282",
  quality_agent      = "#2F855A",
  pr_agent           = "#C05621",
  social_media_agent = "#B83280",
  pr_intern_agent    = "#D69E2E",
  intern_agent       = "#718096",
  judge_agent        = "#9B2C2C"
)

channel_palette <- c(
  comms_huddle    = "#90CDF4",
  one_on_one_chat = "#F6AD55",
  side_huddle     = "#FBD38D",
  official_post   = "#68D391",
  personal_post   = "#FC8181",
  anonymous_post  = "#742A2A"
)

# Channel accountability ladder. rank 1 is most accountable (visible to the
# Judge), rank 6 least. "monitored" marks the two channels the Judge can see.
# The bypass section treats a post on an unmonitored public channel as a
# candidate when the agent left no monitored-channel trace nearby.
channel_hierarchy <- tibble::tibble(
  channel   = c("comms_huddle","one_on_one_chat","side_huddle",
                "official_post","personal_post","anonymous_post"),
  rank      = 1:6,
  layer     = c("Internal","Internal","Internal","Public","Public","Public"),
  monitored = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
)

# Recipients are recorded with short names (legal, pr) while agent_id uses full
# names (legal_agent, pr_agent). This lookup maps short to full so network edges
# connect to real nodes. From report Section 4.3.
recipient_to_agent <- c(
  legal          = "legal_agent",
  platform_trust = "quality_agent",
  pr             = "pr_agent",
  social_manager = "social_media_agent",
  pr_intern      = "pr_intern_agent",
  intern         = "intern_agent",
  judge          = "judge_agent"
)

monitored_channels   <- channel_hierarchy$channel[channel_hierarchy$monitored]
public_channels      <- channel_hierarchy$channel[channel_hierarchy$layer == "Public"]
public_unmonitored   <- channel_hierarchy$channel[channel_hierarchy$layer == "Public" &
                                                   !channel_hierarchy$monitored]

# Topic keyword list (report Section 3.5). Editable inside the topic section.
topic_patterns_default <- tibble::tribble(
  ~topic,           ~pattern,
  "Legal",          "\\b(legal|counsel|consent|language|regulator|attorney|liability)\\b",
  "Product trust",  "\\b(governance|audit|score|data|platform|resident|algorithm|reforms)\\b",
  "Merger",         "\\b(merger|civicloom|harborcrest|acquisition|deal)\\b",
  "Media",          "\\b(saltwind|press|journalist|story|sentiment|denial)\\b",
  "Market",         "\\b(market|stock|investor|residentiq|share|institutional)\\b",
  "Compliance",     "\\b(embargo|monitoring|judge|compliance|oversight|risk|policy)\\b"
)

topic_colours <- c(
  "Legal"          = "#4A90D9",
  "Product trust"  = "#27AE60",
  "Merger"         = "#E67E22",
  "Media"          = "#8E44AD",
  "Market"         = "#16A085",
  "Compliance"     = "#C0392B"
)

n_rounds <- max(messages_tbl$round_idx)

# --- Precompute: how unusual is each agent-channel pairing in the calm period?
# Used by the abnormality shading in Section A. Computed per chosen baseline at
# runtime, so this only defines the helper, not a fixed table.
agent_channel_baseline <- function(df, last_base) {
  base <- df |> filter(round_idx <= last_base)
  total_by_agent <- base |> count(agent_id, name = "agent_total")
  base |>
    count(agent_id, channel, name = "n") |>
    left_join(total_by_agent, by = "agent_id") |>
    mutate(share = n / agent_total)   # share of this agent's calm-period traffic
}

# Source every section module.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
