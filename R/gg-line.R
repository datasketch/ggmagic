
#' @export
gg_line <- function(data, dic = NULL, vars = NULL, ...) {
  opts <- dsopts::dsopts_merge(...)
  data_opts <- list(agg = opts$agg)#dsopts_merge(opts, categories = "dataprep")
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  color_opts <- dsopts_merge(..., categories = "colorprep")
  hdtype <-  data_prep$hdtype
  vars <- data_prep$vars
  data <- gg_data_color(data = data_prep$data, opts = color_opts, viz = "line")

  opts_line <- NULL

  gg <- gg_basic_lines(data = data,
                         x_col = vars$var_dat,
                         y_col = vars$var_num,
                         fill = vars$var_cat,
                         opts = opts_line) |>
    gg_add_text(viz = "line", opts = list(datalabel_show = FALSE)) |>
    gg_color(opts = color_opts, data = data, viz = "line") +
    gg_theme(dsopts_merge(..., categories = "theme"))
  gg
}


#' @export
gg_line_Dat <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  vars <- dic |> filter(hdtype %in% "Dat") %>% .$id
  vars <- vars[1]
  gg_line(data = data, dic = dic, vars = vars, agg = "count", ...)
}

#' @export
gg_line_DatNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_dat <- dic |> filter(hdtype %in% "Dat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_dat[1], var_num[1])
  gg_line(data = data, dic = dic, vars = vars, ...)
}

#' @export
gg_line_CatDatNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_dat <- dic |> filter(hdtype %in% "Dat") %>% .$id
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[1], var_dat, var_num[1])
  gg_line(data = data, dic = dic, vars = vars, color_by = var_cat[1], ...)
}

#' @export
gg_line_CatYeaNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_yea <- dic |> filter(hdtype %in% "Yea") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(  var_cat[1], var_yea[1],, var_num[1])
  gg_line(data = data, dic = dic, vars = vars, color_by = var_cat[1], ...)
}



