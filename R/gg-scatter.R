
#' @export
gg_scatter <- function(data, dic = NULL, vars = NULL, ...) {
  opts <- dsopts::dsopts_merge(...)
  data_opts <- list(agg = opts$agg)#dsopts_merge(opts, categories = "dataprep")
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  color_opts <- dsopts_merge(..., categories = "colorprep")
  hdtype <-  data_prep$hdtype
  vars <- data_prep$vars
  data <- gg_data_color(data = data, opts = color_opts, viz = "scatter")

  opts_scatter <- NULL

  gg <- gg_basic_scatter(data = data,
                         x_col = vars$var_num[1],
                         y_col = vars$var_num[2],
                         fill =  vars$var_cat,
                         opts = opts_scatter) |>
    gg_add_text(viz = "scatter", opts = list(datalabel_show = FALSE)) |>
    gg_color(opts = color_opts, data = data, viz = "scatter") +
    gg_theme(dsopts_merge(..., categories = "theme"))
  gg
}

#' @export
gg_scatter_NumNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_num[1], var_num[2])
  gg_scatter(data = data, dic = dic, vars = vars, ...)
}

#' @export
gg_scatter_CatNumNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[1], var_num[1], var_num[2])
  gg_scatter(data = data, dic = dic, vars = vars, color_by = var_cat[1], ...)
}
