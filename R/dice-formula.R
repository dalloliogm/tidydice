#' Helper function to parse a dice formula 
#'
#' @param dice_formula_part A split dice formula, e.g. 1d6e2. For more complex formula, e.g. 1d6e2+3d4, see parse_dice_formula
#' @import dplyr
#' @import stringr

parse_dice_formula_part <- function(dice_formula_part){

  # define variables to pass CRAN checks
  value <- NULL

  dice_base = str_match_all(
    string=dice_formula_part,
    pattern="^([+-/*]?)(\\d*)?([dD]*)(\\d*)") 
  
  dice_base <- dice_base[[1]]
  
  dice_base <- dice_base %>% 
    tibble::as_tibble(.name_repair="minimal") %>%
    purrr::set_names(c("raw_set", "sign", "operator", "selector", "value")) %>%
    mutate(value=as.numeric(value)) %>%
    select(-sign) %>%
    mutate(
      value = case_when(
        is.na(value) ~ as.numeric(operator),
        T ~ value),
      operator = case_when(
        operator == "" ~ "1",
        T ~ operator
      )
    )

  dice_filters = str_match_all(
    string=dice_formula_part, 
    pattern="([kKeEpP]|rr|ro|ra|mi|ma)([HhlL><]*)(\\d*)") 
  
  dice_filters <- dice_filters[[1]]
  
  dice_filters <- dice_filters %>% 
    tibble::as_tibble(.name_repair="minimal") %>%
    purrr::set_names(c("raw_set", "operator", "selector", "value")) %>%
    mutate(value=as.numeric(value))
  
  bind_rows(
    dice_base,
    dice_filters
  ) 
}

#' Given a dice formula string, split it and return a dataframe with the list 
#' of functions.
#'
#' This is the main function to parse a string containing complex formula 
#' specifications for rolling dice.
#' 
#' The input can be a string containing specifications for multiple dice, e.g.:
#' - 1d6e6          -> roll 1 six-sided dice, explode on 6
#' - 1d6e6+2d4-1d10 -> Roll 1 six-sided dice, explode on 6, plus two 4-sided 
#'                    dice, subract one 10-sided dice
#' 
#' 
#' This is inspired by Avrae's bot syntax for rolling dice. See https://github.com/avrae/d20
#' 
#' @param dice_formula A string containing a dice formula, e.g. 1d6e2+1d4
#' @importFrom tidyr unnest
#' @importFrom tibble tibble rownames_to_column
#' @import stringr
#' @export

parse_dice_formula <- function(dice_formula) {

  # define variables to pass CRAN checks
  subgroup_formula <- NULL
  subgroup_sign <- NULL
  subgroup_id <- NULL
  parts <- NULL
  
  # To simplify Regex parsing, Remove whitespaces and add a default "+"
  dice_formula = str_replace_all(dice_formula, "\\s", "")
  dice_formula = 
    ifelse(is.na(str_match(dice_formula, "^[+-/*]")[1]), 
                        paste0("+", dice_formula),
                        dice_formula)
  
  # Split dice_formula, then parse each substring
  tibble::tibble(subgroup_formula = str_split(dice_formula, 
        "([+-/*])", simplify=T)[-1],
        subgroup_sign = str_extract_all(dice_formula, "([+-/*])")[[1]]) %>%
      unnest(c(subgroup_formula, subgroup_sign)) %>%
      tibble::rownames_to_column("subgroup_id") %>%
      rowwise %>% 
      mutate(
         parts = list(parse_dice_formula_part(subgroup_formula))) %>% 
      mutate(
        subgroup_id = as.integer(subgroup_id), 
        subgroup_formula = paste0(subgroup_sign, subgroup_formula)) %>%
      unnest(parts)

}

# roll_dice_part <- function(current_set = c(), specs=c()){
#   
# }

#' Simulating rolling a dice, using a formula
#' 
#' @param data Data from a previous experiment
#' @param dice_formula Dice formula (e.g. "1d6" = 1 dice with 6 sides).
#'   Exploding dice use Avrae's syntax: `e` (explode on the highest side), 
#'   `e6` (explode on a 6), `e>5` (on 5 or more), `e<2` (on 2 or less). 
#'   A die that explodes is rolled again and the new die is added to the set 
#'   (and can explode itself).
#' @param times How many times a dice is rolled (or how many dice are rolled at the same time)
#' @param rounds Number of rounds 
#' @param success Which result is a success (default = 6)
#' @param agg If TRUE, the result is aggregated (by experiment, rounds) (not implemented)
#' @param prob Vector of probabilities for each side of the dice
#' @param seed Seed to produce reproducible results
#' @param label Custom text to distinguish an experiment, can be used for plotting etc.
#' @return Result of experiment as a tibble
#' @import stringr
#' @export
#' @examples
#' # roll one 6-sided dice
#' roll_dice_formula(dice_formula = "1d6")
#' 
#' # roll one 8-sided dice
#' roll_dice_formula(dice_formula = "1d8")
#' 
#' # roll two 6-sided dice
#' roll_dice_formula(dice_formula = "2d6")
#' 
#' # roll two 6-sided dice, explode dice on a 6
#' roll_dice_formula(dice_formula = "2d6e6")
#' 
#' # roll three 6-sided dice, keep highest 2 rolls
#' roll_dice_formula(dice_formula = "3d6kh2")
#' 
#' # roll three 6-sided dice, keep lowest 2 rolls
#' roll_dice_formula(dice_formula = "3d6kl2")
#' 
#' # roll four 6-sided dice, keep highest 3 rolls, but explode on a 6
#' roll_dice_formula(dice_formula = "4d6kh3e6")
#' 
#' # explode on 5 or more, or on 2 or less
#' roll_dice_formula(dice_formula = "2d6e>5")
#' roll_dice_formula(dice_formula = "2d6e<2")
#' 
#' # roll one 20-sided dice, and add 4
#' roll_dice_formula(dice_formula = "1d20+4")
#' 
#' # roll one 4-sided dice and one 6-sided dice, and sum the results
#' roll_dice_formula(dice_formula = "1d4+1d6")

roll_dice_formula <- function(data=NULL,
                              dice_formula = "1d6", 
                              times = 1, 
                              rounds = 1,
                              seed=NULL, 
                              prob=NULL, 
                              success = c(6),
                              agg=FALSE,
                              label=NULL
                              ) {
  assertthat::assert_that(is.character(dice_formula), msg = "dice_formula must be character")

  # check seed parameter
  if (!missing(seed)) {
    set.seed(seed)
  }
  # check if first parameter is dice_formula instead of data
  if (!missing(data) & is.character(data))  {
    dice_formula <- data
    data <- NULL
    assertthat::assert_that(is.character(dice_formula))
  }
  if (missing(label)) {
    label=dice_formula
  }
  # check if first parameter is times instead of data
  if (!missing(data) & is.numeric(data))  {
    times <- data
    data <- NULL
    assertthat::assert_that(times > 0)
  }

  # define variables to pass CRAN checks
  result <- NULL
  experiment_id <- NULL
  experiment <- NULL
  nr <- NULL
  
  # Parse Dice Type (1d6)
  dicestr1 = str_match(string=dice_formula, pattern="(\\d*)?[dD](\\d*)")
  assertthat::assert_that(is.character(dicestr1[1,1]), 
                          msg = "dice_formula need to contain at least one d statement")
  dice_count  = as.numeric(dicestr1[1,2])
  if (is.na(dice_count)) {dice_count = 1}
  dice_sides  = as.numeric(dicestr1[1,3])
  assertthat::assert_that(dice_count >= 1, 
                          msg = "cannot roll 0 dice!")
  assertthat::assert_that(dice_sides >= 1, 
                          msg = "cannot roll a d0!")
  
  # Parse Exploding Dice (Avrae syntax): e, e6, e>5, e<2
  dice_explode = parse_explode(dice_formula, dice_sides)
  
  # Parse Keep Higher/Lower
  dice_khl = str_match(string=dice_formula, pattern="[kK]([HhlL])(\\d*)")
  dice_khl_sign = case_when(
    is.na(dice_khl[1,2]) ~ T,
    dice_khl[1,2] %in% c("h", "H") ~ T,
    dice_khl[1,2] %in% c("l", "L") ~ F
  )
  dice_khl_n = as.numeric(ifelse(is.na(dice_khl[1,3]), dice_count, dice_khl[1,3]))
  assertthat::assert_that(dice_khl_n <= dice_count, 
                          msg = "invalid kh/kl formula, can't keep more dice than rolled")
  assertthat::assert_that(dice_khl_n > 0, 
                          msg = "invalid kh/kl formula, can't keep less than 1 die")

  # Parse [+-*/]
  dice_op = str_match(string=dice_formula, pattern="\\s*([+-/*^][*]*)\\s*(\\d*)")
  dice_op_sign = dice_op[1,2]
  dice_op_n = as.numeric(dice_op[1,3])

    n_rolls <- rounds * times
    result_df <- tibble::tibble(
      round = as.integer(rep(1:rounds, each = times)),
      nr    = as.integer(rep(1:times, times = rounds)),
      result = roll_dice_sets(
        n_sets = n_rolls,
        dice_count = dice_count,
        dice_sides = dice_sides,
        prob = prob,
        explode_on = dice_explode,
        keep_n = if (is.na(dice_khl[1,3]) & is.na(dice_khl[1,2])) Inf else dice_khl_n,
        keep_highest = dice_khl_sign
      )
    )
    # arithmetic operations
    result_df = result_df %>%
      mutate(
        result = case_when(
          dice_op_sign == "+" ~ result + dice_op_n,
          dice_op_sign == "-" ~ result - dice_op_n,
          dice_op_sign == "*" ~ result * dice_op_n,
          dice_op_sign == "/" ~ result / dice_op_n,
          dice_op_sign %in% c("^", "**") ~ result ** dice_op_n,
          T ~ as.numeric(result),
        )
      )
      
  # Compute success
  result_df = result_df %>%
    mutate(success = result %in% success)
  
  # Format the result df before returning it
  result_df = result_df %>% 
    mutate(experiment_id = 1,
           dice_formula = dice_formula,
           label=label) %>%
    select(experiment_id, dice_formula, label, round, nr, result, success) 
  
  if (missing(data))  {
    
    # result of roll_dice (first experiment)
    result_df <- result_df %>% 
      dplyr::mutate(experiment = as.integer(1)) %>% 
      dplyr::select(experiment, dplyr::everything()) %>%
      dplyr::select(-experiment_id)
    
  } else {
    
    # existing experiment variable?
    max_experiment <- 0L
    if ("experiment" %in% names(data)) {
      max_experiment <- max(data$experiment)
    }
    
    # new experiment (+1)
    result_df <- result_df %>% 
      dplyr::mutate(experiment = as.integer(max_experiment + 1))
    
    # bind result to data (pipe)
    result_df <- dplyr::bind_rows(data, result_df) %>% 
      dplyr::select(experiment, dplyr::everything()) %>%
      dplyr::select(-experiment_id)
  }
  
  # agg?
  if (agg)  {
    
    result_df <- result_df %>%
      dplyr::group_by(experiment, round) %>%
      summarize(
        dice_formula = min(dice_formula),
        times = n(),
        success = as.integer(sum(success))
      )
  }
  
  # return data frame
  result_df
  
}

#' Helper function to parse the exploding dice modifier of a dice formula
#'
#' Supports the syntax of Avrae's d20 library: `e` (explode on the highest
#' side), `e6` (explode on a 6), `e>5` (explode on 5 or more) and `e<2`
#' (explode on 2 or less).
#'
#' @param dice_formula A string containing a dice formula, e.g. 1d6e>5
#' @param dice_sides Number of sides of the dice
#' @return Integer vector of the sides that make a die explode (empty if the
#'   formula has no exploding modifier)
#' @import stringr

parse_explode <- function(dice_formula, dice_sides) {

  m <- str_match(string = dice_formula, pattern = "[eE]([<>]?)(\\d*)")
  if (is.na(m[1, 1])) {
    return(integer(0))
  }

  comparator <- m[1, 2]
  value <- if (m[1, 3] == "") dice_sides else as.numeric(m[1, 3])

  assertthat::assert_that(value >= 1 & value <= dice_sides, 
                          msg = "invalid exploding dice specification")
  
  sides <- seq_len(dice_sides)
  explode_on <- switch(comparator,
                       ">" = sides[sides >= value],
                       "<" = sides[sides <= value],
                       value)
  
  assertthat::assert_that(length(explode_on) < dice_sides,
                          msg = "invalid exploding dice specification, every side would explode")
  explode_on
}

#' Helper function to roll sets of dice, with exploding dice and keep high/low
#'
#' Every die is rolled individually. A die that lands on one of the 
#' `explode_on` sides adds another die to the set, which can explode again.
#' The kept dice are chosen after all explosions, as in Avrae's d20 library.
#'
#' @param n_sets Number of independent sets of dice to roll
#' @param dice_count Number of dice in each set
#' @param dice_sides Number of sides of each die
#' @param prob Vector of probabilities for each side of the dice (or NULL)
#' @param explode_on Sides that make a die explode
#' @param keep_n Number of dice to keep (Inf keeps all, including exploded dice)
#' @param keep_highest Keep the highest (TRUE) or the lowest (FALSE) dice
#' @return Numeric vector with the sum of the kept dice of each set

roll_dice_sets <- function(n_sets, dice_count, dice_sides, prob = NULL, 
                           explode_on = integer(0), 
                           keep_n = Inf, keep_highest = TRUE) {
  
  roll <- function(n) {
    sample(x = seq_len(dice_sides), size = n, replace = TRUE, prob = prob)
  }
  
  sets <- split(roll(n_sets * dice_count), rep(seq_len(n_sets), each = dice_count))
  
  if (length(explode_on) > 0) {
    sets <- lapply(sets, function(dice) {
      pending <- dice
      while (length(pending) > 0) {
        pending <- roll(sum(pending %in% explode_on))
        dice <- c(dice, pending)
      }
      dice
    })
  }
  
  vapply(sets, top_n_dice, numeric(1), n = keep_n, dec = keep_highest, 
         USE.NAMES = FALSE)
}

#' Helper function to get sum of top n dice 
#'
#' @param x Vector of dice-values
#' @param n Number of dice
#' @param dec Decreasing

top_n_dice = function(x, n, dec=F) {
  sum(utils::head(x[order(x, decreasing = dec)], n), na.rm=T)
}
