test_that("Treemap plot", {
  gg_treemap(data = iris, dic = NULL, vars = c("species", "petal_width"))
  gg_treemap_Cat(iris)
  gg_treemap_CatNum(iris)
})
