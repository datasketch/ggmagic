

#' @export
gg_theme <- function(opts = NULL){
  # str(opts)
  plot_margin_bottom <- 8
  if(opts$branding_include)
    plot_margin_bottom <- 20
  title_bottom <- 15
  subtitle_bottom <- 6
  # if(opts$has_subtitle){a
  #   title_bottom <- 3
  #   subtitle_bottom <- 15
  # }

  # sysfonts::font_add_google(name = opts$text_family, family = opts$text_family)
  # sysfonts::font_add_google(name = opts$legend_family, family = opts$legend_family)
  # sysfonts::font_add_google(name = opts$title_family, family = opts$title_family)
  showtext::showtext_auto()

  orientation <- opts$bar_orientation %||% "ver"
  if (orientation == "hor") {
    grid_size_y_orientation <- opts$grid_x_size
    grid_size_x_orientation <- opts$grid_y_size
    axis_line_y_size_orientation <- opts$axis_line_x_size
    axis_line_x_size_orientation <- opts$axis_line_y_size
    axis_line_x_colour_orientation = opts$axis_line_y_color
    axis_line_y_colour_orientation = opts$axis_line_x_color
    axis_title_y_color_orientation = opts$axis_title_x_colour
    axis_title_x_color_orientation = opts$axis_title_y_colour
    axis_text_x_colour_orientation = opts$axis_text_y_color
    axis_text_y_colour_orientation = opts$axis_text_x_color
    axis_title_x_colour_orientation = opts$axis_title_y_color
    axis_title_y_colour_orientation = opts$axis_title_x_color
    grid_x_line_type_orientation = opts$grid_y_line_type
    grid_y_line_type_orientation = opts$grid_x_line_type
    grid_x_color_orientation = opts$grid_y_color
    grid_y_color_orientation = opts$grid_x_color
  } else {
    grid_size_y_orientation <- opts$grid_y_size
    grid_size_x_orientation <- opts$grid_x_size
    axis_line_y_size_orientation <- opts$axis_line_y_size
    axis_line_x_size_orientation <- opts$axis_line_x_size
    axis_line_x_colour_orientation = opts$axis_line_x_color
    axis_line_y_colour_orientation = opts$axis_line_y_color
    axis_title_y_color_orientation = opts$axis_title_y_colour
    axis_title_x_color_orientation = opts$axis_title_x_colour
    axis_text_x_colour_orientation = opts$axis_text_x_color
    axis_text_y_colour_orientation = opts$axis_text_y_color
    axis_title_x_colour_orientation = opts$axis_title_x_color
    axis_title_y_colour_orientation = opts$axis_title_y_color
    grid_x_line_type_orientation = opts$grid_x_line_type
    grid_y_line_type_orientation = opts$grid_y_line_type
    grid_x_color_orientation = opts$grid_x_color
    grid_y_color_orientation = opts$grid_y_color
  }


  grid_line_type <- opts$grid_line_type

  if (grid_line_type == "Solid") {
    grid_line_type <- 1
  } else if (grid_line_type  == "Dot") {
    grid_line_type <- 3
  } else if (grid_line_type  == "Dash") {
    grid_line_type <- 2
  } else if (grid_line_type  == "DashDot") {
    grid_line_type <- 4
  } else if (grid_line_type  == "LongDash") {
    grid_line_type <- 5
  } else if (grid_line_type  == "ShortDash") {
    grid_line_type <- 8
  } else {
    grid_line_type <- 1
  }

  thm <- list(
    line_colour = opts$line_color,
    line_size = opts$line_size,
    rect_colour = opts$rect_color %||% opts$background_color,
    text_colour = opts$text_color,
    text_size = opts$text_size,
    axis_text_colour = opts$axis_text_color %||% opts$text_color,
    axis_line_size = opts$axis_line_size %||% opts$line_size,
    axis_line_x_size = axis_line_x_size_orientation  %||% opts$axis_line_size %||% opts$line_size,
    axis_line_y_size = axis_line_y_size_orientation  %||% opts$axis_line_size %||% opts$line_size,
    axis_line_colour = opts$axis_line_color %||% opts$line_color,
    axis_line_x_colour = axis_line_x_colour_orientation %||% opts$axis_line_color %||% opts$line_color,
    axis_line_y_colour = axis_line_y_colour_orientation %||% opts$axis_line_color %||% opts$line_color,
    axis_text_x_colour = axis_text_x_colour_orientation %||% opts$text_color,
    axis_text_y_colour = axis_text_y_colour_orientation %||% opts$text_color,
    axis_title_colour = opts$axis_title_color %||% opts$text_color,
    axis_title_x_colour = axis_title_x_colour_orientation %||% opts$text_color,
    # axis_title_x_angle = opts$axis_title_x_angle,
    axis_title_y_colour = axis_title_y_color_orientation %||% opts$text_color,
    # axis_title_y_angle = opts$axis_title_y_angle,
    axis_ticks_colour = opts$axis_ticks_color %||% opts$line_color, # transparent
    legend_background_colour = opts$legend_background_color %||% opts$background_color,
    legend_background_fill = opts$legend_background_fill %||% opts$background_color,
    legend_key_colour = opts$legend_key_color %||% opts$background_color,
    legend_key_fill = opts$legend_key_fill %||% opts$background_color,
    legend_text_colour = opts$legend_text_color %||% opts$text_color,
    legend_position = opts$legend_position %||% "right",
    panel_background_fill = opts$panel_background_fill %||% opts$background_color,
    panel_border_size = opts$panel_border_size %||% opts$line_size,
    # panel_border_colour = opts$panel_border_color,
    grid_size = opts$grid_size %||% opts$line_size,
    grid_x_size = grid_size_x_orientation %||% opts$grid_size,
    grid_y_size = grid_size_y_orientation %||% opts$grid_size,
    grid_line_type = grid_line_type,
    grid_x_line_type = opts$grid_x_line_type %||% grid_line_type,
    grid_y_line_type = opts$grid_y_line_type %||% grid_line_type,
    grid_color = opts$grid_color,
    grid_x_color = opts$grid_x_color %||% opts$grid_color,
    grid_y_color = opts$grid_y_color %||% opts$grid_color,
    # panel_grid_major_colour = opts$panel_grid_major_color %||% opts$grid_color,
    # panel_grid_minor_colour = opts$panel_grid_minor_color %||% opts$grid_color,
    strip_background_fill = opts$strip_background_fill %||% opts$accent_color,
    plot_background_colour = opts$plot_background_color %||% opts$background_color,
    plot_background_fill = opts$plot_background_fill%||% opts$background_color,
    plot_title_family = opts$plot_title_family %||% opts$title_family,
    plot_title_colour = opts$plot_title_color %||% opts$text_color,
    plot_title_hjust = opts$plot_title_hjust %||% 0,
    plot_subtitle_family = opts$plot_subtitle_family %||% opts$text_family,
    plot_subtitle_colour = opts$plot_subtitle_color %||% opts$text_color,
    plot_subtitle_hjust = opts$plot_subtitle_hjust %||% 0,
    plot_caption_family = opts$plot_caption_family %||% opts$text_family,
    plot_caption_colour = opts$plot_caption_color %||% opts$text_color,
    plot_caption_hjust = opts$plot_title_hjust %||% 0
  )
  # message("thm")
  # str(thm)


  theme(
    line = element_line(
      colour = thm$line_colour,
      size = thm$line_size,
      linetype = 1,
      lineend = "butt"),
    rect = element_rect(
      fill = "white",
      colour = thm$rect_colour,
      size = 0.5,
      linetype = 1),
    text = element_text(
      debug=FALSE,
      margin=margin(),
      family = opts$text_family,
      face = "plain",
      colour = thm$text_colour,
      size = thm$text_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 1.2),
    axis.text = element_text(
      debug=FALSE,
      margin=margin(6, 0, 6, 0),
      size = rel(0.8),
      colour = thm$axis_text_color),
    axis.line = element_line(
      colour = thm$axis_line_colour,
      size = thm$axis_line_size
      # size = rel(0.5)
    ),
    axis.line.x = element_line(
      colour = thm$axis_line_x_colour,
      size = thm$axis_line_x_size),
    axis.line.y = element_line(
      colour = thm$axis_line_y_colour,
      size = thm$axis_line_y_size),
    axis.text.x = element_text(
      debug=FALSE,
      # margin=margin(6, 0, 6, 0),
      vjust = 1,
      colour = thm$axis_text_x_colour,
      angle = thm$axis_text_x_angle,
      face='plain'),
    axis.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      hjust = 1,
      colour = thm$axis_text_y_colour,
      angle = thm$axis_text_y_angle,
      face='plain'),
    axis.title = element_text(
      face='plain',
      lineheight = 1.5,
      colour = thm$axis_title_colour),
    axis.title.x = element_text(
      debug=FALSE,
      colour = thm$axis_title_x_colour,
      margin=margin(0,0,3,0),
      size = rel(0.9),
      vjust=1),
    axis.title.y = element_text(
      debug=FALSE,
      colour = thm$axis_title_y_colour,
      # margin=margin(),
      angle = 90,
      size = rel(0.9),
      vjust=1),
    axis.ticks = element_line(
      colour = thm$axis_ticks_colour %||% thm$axis_line_colour,
      size = 0.3),
    # axis.ticks.length = grid::unit(0.15, "cm"),
    # axis.ticks.length.x.bottom = grid::unit(0.15, "cm"),
    # axis.ticks.length.x.top = grid::unit(0.15, "cm"),
    # axis.ticks.length.y.left = grid::unit(0.15, "cm"),
    # axis.ticks.length.y.right = grid::unit(0.15, "cm"),
    legend.background = element_rect(
      colour = thm$legend_background_colour,
      fill = thm$legend_background_fill),
    #legend.margin = #grid::unit(0.2 * spacing, "cm"),
    legend.key = element_rect(
      colour = thm$legend_key_colour,
      fill = thm$legend_key_fill),
    legend.key.size = grid::unit( 1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.title = element_text(
      colour = thm$legend_text_colour,
      margin=margin(),
      size = rel(0.9)
    ),
    legend.text = element_text(
      debug=FALSE,
      colour = thm$legend_text_colour,
      margin=margin(),
      size = rel(0.8)),
    legend.position = opts$legend_position,
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(
      fill = thm$panel_background_fill,
      colour = NA),
    panel.border = element_rect(
      size = thm$panel_border_size,
      fill = 'transparent',
      colour = thm$panel_border_colour),
    panel.grid.major = element_line(
      size = thm$grid_size,
      linetype = thm$grid_line_type,
      colour = thm$grid_color),
    panel.grid.major.x = element_line(
      size = thm$grid_x_size,
      linetype = thm$grid_x_line_type,
      colour = thm$grid_x_color),
    panel.grid.major.y = element_line(
      size = thm$grid_y_size,
      linetype = thm$grid_y_line_type,
      colour = thm$grid_y_color),
    panel.grid.minor = element_line(
      linetype= thm$grid_line_type,
      size = thm$grid_size,
      colour = thm$grid_color),
    panel.grid.minor.x = element_line(
      size = thm$grid_x_size,
      linetype = thm$grid_x_line_type,
      colour = thm$grid_x_color),
    panel.grid.minor.y = element_line(
      size = thm$grid_y_size,
      linetype = thm$grid_y_line_type,
      colour = thm$grid_y_color),
    # panel.margin = grid::unit(0.5 * spacing, 'cm'),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,
    strip.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    strip.background = element_rect(
      fill = thm$strip_background_fill,
      colour = NA),
    strip.text.x = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(1),
      face = 'plain'),
    strip.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = -90,
      face = 'plain',
      size = rel(1)),
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    plot.background = element_rect(
      colour = thm$plot_background_colour,
      fill = thm$plot_background_fill),
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      # plot.title = ggtext::element_markdown(
      debug=FALSE,
      family = thm$plot_title_family,
      colour = thm$plot_title_colour,
      margin=margin(6, -3, title_bottom, 0),
      size = rel(1.2),
      hjust = thm$plot_title_hjust,
      vjust = 1,
      face='plain'),
    plot.subtitle = element_text(
      debug=FALSE,
      family = thm$plot_subtitle_family,
      colour = thm$plot_subtitle_colour,
      margin=margin(3, 0, subtitle_bottom, 0),
      size = rel(1),
      hjust = thm$plot_subtitle_hjust %||% thm$plot_title_hjust,
      vjust = 1,
      face='plain'),
    plot.caption.position = "plot",
    plot.caption = ggtext::element_markdown(
      debug=FALSE,
      family = thm$plot_subtitle_family,
      colour = thm$plot_caption_colour,
      margin=margin(3, 0, 8, 0),
      size = rel(0.8),
      hjust = thm$plot_caption_hjust,
      vjust = 1,
      valign = 0.5,
      align_heights = TRUE,
      lineheight = 1.5,
      face='plain'),
    plot.margin = margin(15, 20, plot_margin_bottom, 20),
    complete = TRUE
  )
}


#' @export
gg_clean_theme<- function(opts = NULL){
  gg_theme(opts = opts) + theme(
    axis.line=element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major=element_blank())
}
