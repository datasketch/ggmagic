test_that("Treemap plot", {
  gg_treemap(data = iris, dic = NULL, vars = c("species", "petal_width"))
  gg_treemap_Cat(iris)
  gg_treemap_CatNum(iris)
  data <- diamonds |> select(cut, color, x)
  data$cut <- as.character(data$cut)
  data$color <- as.character(data$color)
  gg_treemap_CatCatNum(data = data)
})
