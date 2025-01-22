box::use(
  bslib[bs_theme],
)

#' @export
app_theme <- bs_theme(
  version = 5,
  bootswatch = "lux",
  # Colors
  primary = "#4a57a6",
  secondary = "#4192b5",
  # Typography
  font_scale = 0.9,
  # Cards
  card_bg = "#ffffff",
  card_border_width = 0,
  card_border_radius = "0.5rem",
  # Custom properties
  "navbar-bg" = "#ffffff",
  "navbar-padding-y" = "1rem",
  # Add Google Fonts
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Poppins")
)
