
#' @export
gg_treemap <- function(data, dic = NULL, vars = NULL, ...) {
  opts <- dsopts::dsopts_merge(...)
  data_opts <- list(agg = opts$agg)#dsopts_merge(opts, categories = "dataprep")
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  color_opts <-  dsopts_merge(..., categories = "colorprep")
  hdtype <-  data_prep$hdtype
  vars <- data_prep$vars
  data <-  data_prep$data
  data <- gg_data_color(data = data, opts = color_opts, viz = "treemap")

  opts_treemap <- NULL

  gg <- gg_basic_treemap(data = data,
                     x_col = vars$var_cat,
                     y_col = vars$var_num,
                     fill = vars$var_fill,
                     opts = opts_treemap) |>
    gg_add_text(viz = "treemap", opts = list(datalabel_show = FALSE)) |>
    gg_color(opts = color_opts, data = data, viz = "treemap") +
    gg_clean_theme(dsopts_merge(..., categories = "theme"))
  gg
}




#' @export
gg_treemap_Cat <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  vars <- dic |> filter(hdtype %in% "Cat") %>% .$id
  vars <- vars[1]
  gg_treemap(data = data, dic = dic, vars = vars, ..., agg = "count")
}

#' @export
gg_treemap_CatNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[1], var_num[1])
  gg_treemap(data = data, dic = dic, vars = vars, ...)
}

#' @export
gg_treemap_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[2], var_cat[1], var_num[1])
  gg_treemap(data = data, dic = dic, vars = vars, color_by = var_cat[1], ...)
}

#' @export
gg_treemap_CatYeaNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_yea <- dic |> filter(hdtype %in% "Yea") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c( var_yea[1], var_cat[1], var_num[1])
  gg_treemap(data = data, dic = dic, vars = vars, color_by = var_cat[1], ...)
}


