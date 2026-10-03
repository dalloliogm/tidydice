context("plot_dice")
library(tidydice)

# draw the plot, return warnings (a ggplot only warns when it is drawn)
draw_warnings <- function(p) {
  warnings <- character()
  withCallingHandlers(
    ggplot2::ggsave(tempfile(fileext = ".png"), p, width = 3, height = 2, dpi = 30),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  warnings
}

test_that("plot_dice draws results that are not a dice face", {
  # sums, results above 6 or below 1, decimals: no warnings
  expect_length(draw_warnings(plot_dice(roll_dice_formula("3d6", times = 3, seed = 1))), 0)
  expect_length(draw_warnings(plot_dice(roll_dice_formula("1d20", times = 3, seed = 1))), 0)
  expect_length(draw_warnings(plot_dice(roll_dice_formula("1d6-3", times = 6, seed = 1))), 0)
  expect_length(draw_warnings(plot_dice(roll_dice_formula("1d6/4", times = 6, seed = 1))), 0)
  expect_length(draw_warnings(plot_dice(roll_dice(times = 4, sides = 8, seed = 5))), 0)
  expect_length(draw_warnings(plot_dice(roll_dice_formula("3d6", times = 3, seed = 1), 
                                        detailed = TRUE)), 0)
  
  # a decimal is not drawn as the face of its integer part
  p <- plot_dice(roll_dice_formula("1d1/2"))
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% layers)
  
  # faces 1..6 are still drawn with dots, without a number
  p <- plot_dice(roll_dice_formula("1d1+2"))
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomText" %in% layers)
})
