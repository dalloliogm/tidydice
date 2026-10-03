context("dice_formula")
library(tidydice)


test_that("parse_dice_formula works correctly", {
  
  dice_formula = "11d44"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+11d44", "+", "11d44", "11", "d", 44
  )
  expect_equal(formula_df, expected_output)
  
  dice_formula = "-2d9"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "-2d9", "-", "2d9", "2", "d", 9
  )
  expect_equal(formula_df, expected_output)
  
  # one group, two operations
  dice_formula = "1d5rr>2"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+1d5rr>2", "+", "1d5", "1", "d", 5,
    1, "+1d5rr>2", "+", "rr>2", "rr", ">", 2
  )
  expect_equal(formula_df, expected_output)
  
  # a formula cannot start with * or /
  expect_error(parse_dice_formula("*1d5"), "cannot parse '\\*1d5'")
  
  # one group, multiple operations
  dice_formula = "1d5e2e3rr<2";   
  formula_df = parse_dice_formula(dice_formula) 
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+1d5e2e3rr<2", "+", "1d5", "1", "d", 5,
    1, "+1d5e2e3rr<2", "+", "e2", "e", "", 2,
    1, "+1d5e2e3rr<2", "+", "e3", "e", "", 3,
    1, "+1d5e2e3rr<2", "+", "rr<2", "rr", "<", 2
  )
  expect_equal(formula_df, expected_output)
  
  # Two groups, with multiple operations
  dice_formula = "1d3+9d11e3rr>6"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+1d3",        "+", "1d3",   "1", "d", 3,
    2, "+9d11e3rr>6", "+", "9d11",  "9", "d", 11,
    2, "+9d11e3rr>6", "+", "e3",    "e", "",   3,
    2, "+9d11e3rr>6", "+", "rr>6",  "rr", ">", 6
  )
  expect_equal(formula_df, expected_output)
  
  dice_formula = "d4-1d5e2+1d2rr3-d2kl2p<4*3d2+9/9+d98"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1,   "+d4",       "+",    "d4",    "1",    "d",   4,
    2,   "-1d5e2",    "-",    "1d5",   "1",    "d",   5,
    2,   "-1d5e2",    "-",    "e2",    "e",    "",   2,
    3,   "+1d2rr3",   "+",    "1d2",   "1",    "d",   2,
    3,   "+1d2rr3",   "+",    "rr3",   "rr",    "",   3,
    4,   "-d2kl2p<4", "-",    "d2",    "1",    "d",   2,
    4,   "-d2kl2p<4", "-",    "kl2",   "k",    "l",   2,
    4,   "-d2kl2p<4", "-",    "p<4",   "p",    "<",   4,
    5,   "*3d2",      "*",    "3d2",   "3",    "d",   2,
    6,   "+9",        "+",    "9",     "9",    "",   9,
    7,   "/9",        "/",    "9",     "9",    "",   9,
    8,   "+d98",      "+",    "d98",   "1",    "d",   98,
  )
  expect_equal(formula_df, expected_output)
  
  # Same as previous, but with spaces
  dice_formula = "d4- 1d5e2+ 1d2rr3   - d2kl2p<4 * 3d2+9/9+d98"
  formula_df = parse_dice_formula(dice_formula)
  expect_equal(formula_df, expected_output)
  
  # Same as previous, but with even more spaces
  dice_formula = "d 4- 1d5 e2+ 1d2 rr3   - d2kl 2 p<4 * 3d2+9 / 9+ d 9 8"
  formula_df = parse_dice_formula(dice_formula)
  expect_equal(formula_df, expected_output)
  
  # Same parser as roll_dice_formula(): d%, several selectors, ^ and **, case
  expect_equal(
    parse_dice_formula("2D%k>3<2^2**3"),
    tibble::tribble(
      ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
      1, "+2D%k>3<2", "+", "2D%",  "2", "d", 100,
      1, "+2D%k>3<2", "+", "k>3",  "k", ">", 3,
      1, "+2D%k>3<2", "+", "k<2",  "k", "<", 2,
      2, "^2",        "^", "2",    "2", "",  2,
      3, "**3",       "**", "3",   "3", "",  3
    ))
  
  # Leading minus, parentheses: groups are listed from left to right
  expect_equal(
    parse_dice_formula("-(1d6+2)*3"),
    tibble::tribble(
      ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
      1, "-1d6", "-", "1d6", "1", "d", 6,
      2, "+2",   "+", "2",   "2", "",  2,
      3, "*3",   "*", "3",   "3", "",  3
    ))
  
  # Only the syntax is checked, not whether the formula can be rolled
  expect_equal(nrow(parse_dice_formula("1d2rr3")), 2)
  
  # Invalid syntax is an error, also here
  expect_error(parse_dice_formula("1d6x3"), "cannot parse 'x3'")
  expect_error(parse_dice_formula("1d6e"), "needs a selector")
  expect_error(parse_dice_formula("(1d6"), "missing closing parenthesis")
  expect_error(parse_dice_formula(c("1d6", "1d4")), "single character string")
  
})

test_that("base dice formula works correctly", {

  expect_equal(nrow(roll_dice_formula("1d6")),  1)
  expect_equal(nrow(roll_dice_formula("12d6")), 1)
  expect_equal(nrow(roll_dice_formula("3D8")), 1)
  expect_equal(nrow(roll_dice_formula("1d46")), 1)
  expect_equal(nrow(roll_dice_formula("d7")),   1)

  expect_error(nrow(roll_dice_formula("0d5")), "cannot roll 0 dice!")
  expect_error(nrow(roll_dice_formula("2d0")), "cannot roll a d0!")  
  
  expect_lt(max(roll_dice_formula("1d6", times=200)$result), 7)
  expect_lt(max(roll_dice_formula("d6",  times=200)$result), 7)
  expect_lt(max(roll_dice_formula("8d6", times=200)$result), 7*8)
  expect_gt(min(roll_dice_formula("8d6", times=200)$result), 7)
    
  expect_gt(mean(roll_dice_formula("1d30", times=2000)$result),
            mean(roll_dice_formula("1d15", times=2000)$result)
  )
  
  expect_gt(mean(roll_dice_formula("2d8", times=200)$result),
            mean(roll_dice_formula("1d8", times=200)$result)
  )
  
  expect_equal(mean(roll_dice_formula("1d6", times=2000)$result), 
               sum(1:6)/6,
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("2d8", times=2000)$result), 
               2*(sum(1:8)/8),
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("d33", times=2000)$result), 
               sum(1:33)/33,
               tolerance=0.2)
  
})


test_that("exploding dice", {
  
  expect_equal(nrow(roll_dice_formula("1d6e6")),  1)
  expect_equal(nrow(roll_dice_formula("1d3E2")),  1)
  
  # Exploding rolls should never be multiple of exploded die
  expect_equal(sum( 
    roll_dice_formula("1d7e7", times=200)$result %% 7 == 0),
    0)  
  
  # roll 1d2e2 200 times, results should never be even
  expect_equal(sum( 
    roll_dice_formula("1d2e2", times=200)$result %% 2 == 0),
    0)  

  # roll 1d6e2 (exploding a dice that is not the maximum)
  expect_equal(2 %in% roll_dice_formula("1d6e2", times=200)$result, F) 
  
  # exploding dice should always be lower or equal to n. sides
  expect_error(roll_dice_formula("1d4e6"), "no side of the dice matches")

  # 1d2e1 is valid (the die stops exploding on a 2), 1d1e1 would explode forever
  expect_equal(mean(roll_dice_formula("1d2e1", times=20000, seed=1)$result), 3, tolerance=0.03)
  expect_error(roll_dice_formula("1d1e1"), "every side would explode")
  expect_error(roll_dice_formula("1d6e>0"), "every side would explode")
  expect_error(roll_dice_formula("1d6e0"), "no side of the dice matches")
   
  # Expected values over many rolls
  expect_equal(
    mean(roll_dice_formula("1d6e6", times=2000)$result),
    sum(1:5)/5 + mean(6*rgeom(2000, 1-1/6)), #4.2
    tolerance=0.5)

  expect_equal(
    mean(roll_dice_formula("1d10e10", times=2000)$result),
    sum(1:9)/9 + mean(10*rgeom(2000, 1-1/10)), #4.2
    tolerance=0.5)
  
  expect_gt(mean(roll_dice_formula("1d6e6", times=2000)$result),
            mean(roll_dice_formula("1d6", times=2000)$result),
  )
  
  # Like in Avrae, "e" needs a selector
  expect_error(roll_dice_formula("1d6e"), "needs a selector")
  
  # Exact expected values: E = 21/5 when exactly one side explodes
  expect_equal(mean(roll_dice_formula("1d6e6", times=20000, seed=1)$result), 
               4.2, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e2", times=20000, seed=1)$result), 
               4.2, tolerance=0.03)
  
  # Comparators are strict, like in Avrae: e>4 explodes on 5 and 6, e<3 on 1 and 2
  expect_equal(mean(roll_dice_formula("1d6e>4", times=20000, seed=1)$result),
               5.25, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e<3", times=20000, seed=1)$result),
               21/4, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e>5", times=20000, seed=1)$result),
               4.2, tolerance=0.03)
  
  # Explosions are rolled per die: 10d6e6 mean is 10 * 4.2
  expect_equal(mean(roll_dice_formula("10d6e6", times=5000, seed=1)$result),
               42, tolerance=0.02)
  
  # Exploded dice can explode again
  expect_true(any(roll_dice_formula("1d2e2", times=2000, seed=1)$result > 3))
  
  # Exploded dice are added to the set, so they compete for kh/kl as separate dice
  expect_gt(mean(roll_dice_formula("2d6e6kh2", times=5000, seed=1)$result),
            mean(roll_dice_formula("2d6kh2", times=5000, seed=1)$result))
  expect_lte(max(roll_dice_formula("2d6e6kh1", times=2000, seed=1)$result), 6)
  
  # prob is honoured by the dice that explode
  expect_true(all(roll_dice_formula("1d3e3", times=50, prob=c(1,0,0))$result == 1))
  }
)


test_that("Keep High/Low dice", {
  expect_gt(mean(roll_dice_formula("3d6kh1", times=200)$result),2)
  
  expect_lt(mean(roll_dice_formula("3d6KH1", times=200)$result),18)
  expect_equal(mean(roll_dice_formula("4d4kH2", times=2000)$result), 6.5, 
               tolerance=0.2)

  expect_equal(mean(roll_dice_formula("6d4Kl3", times=2000)$result), 5, 
               tolerance=0.2)
  
  # D&D Adv/Dis
  expect_equal(mean(roll_dice_formula("1d20", times=2000)$result), 
               sum(1:20)/20, 
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("2d20Kh1", times=2000)$result), 
               13.7, # Can't work out the correct formula for this, right now
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("2d20kL1", times=2000)$result), 
               7.2, 
               tolerance=0.2)
  
  # Invalid formulas
  expect_error(roll_dice_formula("2d6kh3"), "invalid kh/kl formula, can't keep more dice than rolled")
  expect_error(roll_dice_formula("2d6kh0"), "invalid kh/kl formula, can't keep less than 1 die")
  
  expect_gt(mean(roll_dice_formula("2d15kh1", times=200)$result),
            mean(roll_dice_formula("2d15kl1", times=200)$result)
            )
  
  # Exploding + kh/kl
  expect_gt(mean(roll_dice_formula("3d6e6kh2", times=5000, seed=1)$result),
            mean(roll_dice_formula("3d6kh2", times=5000, seed=1)$result))
  
    }  
)

test_that("arithmetic operations", {
  expect_equal(
    mean(roll_dice_formula("1d20+5", times=2000)$result), 
    sum(1:20)/20+5, 
    tolerance=0.2)
  #expect_equal(roll_dice_formula("1d1")$result, 1)
  expect_equal(
    mean(roll_dice_formula("1d18 + 3", times=2000)$result), 
    sum(1:18)/18+3, 
    tolerance=0.2)  
  expect_equal(
    mean(roll_dice_formula("1d20-2", times=2000)$result), 
    sum(1:20)/20-2, 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d10*5", times=2000)$result), 
    5*(sum(1:10)/10), 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d9/3", times=2000)$result), 
    (sum(1:10)/10)/3, 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d10^2", times=2000)$result), 
    (sum((1:10)**2)/10), 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d5**3", times=2000)$result), 
    (sum((1:5)**3)/5), 
    tolerance=0.2)
})
test_that("piping", {
  expect_equal(
    roll_dice_formula("1d5**3", times=20) %>% 
      roll_dice_formula("1d5**3", times=20) %>%
      count(experiment) %>%
      nrow,
    2)
  expect_equal(
    roll_dice_formula("1d5", times=20) %>% 
      roll_dice_formula("2d4", times=20) %>%
      count(dice_formula) %>%
      nrow,
    2)
  expect_equal(
    roll_dice_formula("1d5", times=20) %>% 
      roll_dice_formula("2d4", times=20) %>%
      nrow,
    40)
  expect_equal(
    roll_dice_formula("2d20kh1", times=20, label="adv") %>% 
      roll_dice_formula("2d20kl1", times=20, label="dis") %>%
      count(label) %>%
      nrow,
    2) 
})

test_that("other function parameters", {
  # Check prob
  expect_equal(
    mean(roll_dice_formula("1d3", prob=c(0,0,1))$result),
    3
  )
  expect_error(roll_dice_formula("1d4", prob=c(1,0.2)))
  expect_error(roll_dice_formula("1d4", prob=c(-1,-1,-1,-1)))
  
  # Check random seed
  expect_equal(
    roll_dice_formula("1d1000", seed=1234),
    roll_dice_formula("1d1000", seed=1234)
  )
  # Check random seed
  expect_equal(
    roll_dice_formula("19d1000kh4", seed=1234),
    roll_dice_formula("19d1000kh4", seed=1234)
  )
  # Check random seed
  expect_equal(
    roll_dice_formula("1d1000", seed=456),
    roll_dice_formula("1d1000", seed=456)
  )
  # Check random seed
  expect_equal(
    roll_dice_formula("24d4kl10e4", seed=-23),
    roll_dice_formula("24d4kl10e4", seed=-23)
  )
  expect_equal(
    roll_dice_formula("24d4kl10e4", seed=1)$result != roll_dice_formula("24d4kl10e4", seed=2)$result, 
    T
  )
})


test_that("multiple dice groups and operator precedence", {
  expect_equal(mean(roll_dice_formula("1d6+1d4", times=5000, seed=1)$result), 
               3.5 + 2.5, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("2d6-1d4", times=5000, seed=1)$result), 
               7 - 2.5, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e6+1d4+3", times=20000, seed=1)$result), 
               4.2 + 2.5 + 3, tolerance=0.02)
  expect_equal(mean(roll_dice_formula("-1d4+10", times=5000, seed=1)$result), 
               10 - 2.5, tolerance=0.03)
  
  # * before +, parentheses, ^ before *
  expect_equal(roll_dice_formula("1d1+2*3")$result, 7)
  expect_equal(roll_dice_formula("(1d1+2)*3")$result, 9)
  expect_equal(roll_dice_formula("2*1d1^3")$result, 2)
  expect_equal(roll_dice_formula("2^3^2+1d1")$result, 513)
  expect_equal(roll_dice_formula("2**3+1d1")$result, 9)
  expect_equal(roll_dice_formula("1d1 + 1 d 1")$result, 2)
  expect_error(roll_dice_formula("(1d6+2"), "missing closing parenthesis")
})

test_that("Avrae modifiers", {
  # d20 syntax: k, p, rr, ro, ra, e, mi, ma, d%
  mean_of <- function(f, times = 20000) {
    mean(roll_dice_formula(f, times = times, seed = 1)$result)
  }
  
  # Keep / drop with selectors
  expect_equal(mean_of("4d6kh3"), 12.24, tolerance = 0.01)
  expect_equal(mean_of("4d6pl1"), mean_of("4d6kh3"), tolerance = 0.01)
  expect_equal(mean_of("1d6k>4"), 11/6, tolerance = 0.03) # 5, 6 kept, else 0
  expect_equal(mean_of("1d6k5"), 5/6, tolerance = 0.03)
  expect_equal(mean_of("1d6p<3"), 18/6, tolerance = 0.03) # 1, 2 dropped
  expect_equal(mean_of("1d6p3"), (21-3)/6, tolerance = 0.03)
  
  # Reroll: rr until the die does not match, ro once, ra adds
  expect_equal(mean_of("1d6rr1"), 4, tolerance = 0.01) # uniform on 2..6
  expect_equal(mean_of("1d6rr<3"), 4.5, tolerance = 0.01) # uniform on 3..6
  expect_equal(min(roll_dice_formula("1d6rr<3", times = 500)$result), 3)
  expect_equal(mean_of("1d6ro1"), 3.5 + 1/6*(3.5-1), tolerance = 0.01)
  expect_equal(mean_of("1d6ro<3"), 4/6*4.5 + 2/6*3.5 + 0, tolerance = 0.03) 
  expect_equal(mean_of("1d6ra1"), 3.5 + 3.5/6, tolerance = 0.01)
  # ra explodes at most one die
  expect_equal(max(roll_dice_formula("5d6ra<7", times = 500)$result), 36)
  
  # Min / max
  expect_equal(mean_of("1d6mi3"), (3+3+3+4+5+6)/6, tolerance = 0.01)
  expect_equal(mean_of("1d6ma4"), (1+2+3+4+4+4)/6, tolerance = 0.01)
  expect_equal(min(roll_dice_formula("3d6mi3", times = 200)$result), 9)
  
  # Modifiers are applied in the order they are written
  expect_lte(max(roll_dice_formula("2d6e6kh1", times = 2000, seed = 1)$result), 6)
  expect_equal(mean_of("1d6kh1e6"), 4.2, tolerance = 0.03)
  expect_equal(max(roll_dice_formula("3d6mi4ma4", times = 200)$result), 12)
  
  # Consecutive identical modifiers are merged, like in d20: e5e6 is e(5,6)
  expect_equal(mean_of("1d6e5e6"), 5.25, tolerance = 0.03)
  expect_equal(mean_of("1d6e5e6"), mean_of("1d6e5>5"), tolerance = 0.03)
  expect_error(roll_dice_formula("1d2e1e2"), "every side would explode")
  
  # Several selectors are a union
  expect_equal(mean_of("1d6k1>5"), 7/6, tolerance = 0.03)
  
  # Percentile dice
  expect_equal(mean_of("d%"), 50.5, tolerance = 0.02)
  expect_equal(max(roll_dice_formula("1d%", times = 2000, seed = 1)$result), 100)
  
  # prob is used for every roll, also rerolls
  expect_true(all(roll_dice_formula("1d3rr1", times = 50, prob = c(.5, .5, 0))$result == 2))
  
  # Limits like in d20: no endless loops
  expect_error(roll_dice_formula("1d6rr>0"), "every side would be rerolled")
  expect_error(roll_dice_formula("1000d6e6", times = 2), "Too many dice rolled")
  expect_error(roll_dice_formula("2d6rr7"), "no side of the dice matches")
})

test_that("unsupported or invalid syntax is an error, not ignored", {
  expect_error(roll_dice_formula("1d6x3"), "cannot parse 'x3'")
  expect_error(roll_dice_formula("2d20h1"), "cannot parse 'h1'")
  expect_error(roll_dice_formula("1d6rr"), "needs a selector")
  expect_error(roll_dice_formula("2d6kh"), "needs a selector")
  expect_error(roll_dice_formula("1d6mi>2"), "needs a plain number")
  expect_error(roll_dice_formula("1d6+"), "cannot parse")
  expect_error(roll_dice_formula("5+3"), "at least one d statement")
  expect_error(roll_dice_formula("1d6 [fire]"), "cannot parse")
})
