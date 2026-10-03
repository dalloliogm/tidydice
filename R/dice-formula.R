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
#' @details
#' The syntax is based on [Avrae's d20 library](https://github.com/avrae/d20).
#' Spaces and case are ignored. 
#' 
#' **Dice**: `NdS` rolls N dice with S sides (`d6` = `1d6`, `d%` = `d100`). 
#' Dice can be combined with numbers using `+ - * / ^`, 
#' parentheses and the usual precedence, e.g. `(2d6+3)*2` or `1d8+1d6+2`. 
#' 
#' **Modifiers** follow the dice and are applied in the order they are written.
#' Each needs a selector:
#' 
#' | Modifier | Meaning |
#' |:--|:--|
#' | `k` | keep the dice matching the selector, drop the others |
#' | `p` | drop the dice matching the selector |
#' | `rr` | reroll dice matching the selector, until none match |
#' | `ro` | reroll dice matching the selector once |
#' | `ra` | roll one extra die (once) if a die matches the selector |
#' | `e` | explode: roll an extra die for each die matching the selector, 
#' and repeat for the new dice |
#' | `mi` | minimum: dice below the number are raised to it |
#' | `ma` | maximum: dice above the number are lowered to it |
#' 
#' **Selectors**: `N` (equal to N), `<N` (less than N), `>N` (greater than N), 
#' `hN` (the N highest dice) and `lN` (the N lowest dice). Several selectors can 
#' be combined (`k>3<2`), and so can consecutive identical modifiers 
#' (`e5e6` is the same as `e5>5`). `mi` and `ma` only take a plain number.
#' Only the dice that are not dropped can be selected.
#' 
#' Not supported (yet): comments, lists of numbers like `(1,2,3)kh1`, 
#' functions, `//` and `%` operators. Invalid syntax is an error.
#' A set can't contain more than 1000 dice.
#' 
#' @param data Data from a previous experiment
#' @param dice_formula Dice formula (e.g. "1d6" = 1 dice with 6 sides). 
#'   The syntax follows Avrae's dice bot (see Details).
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
#' # explode on 5 or 6 (">" and "<" are strict, as in Avrae)
#' roll_dice_formula(dice_formula = "2d6e>4")
#' 
#' # roll four 6-sided dice, drop the lowest, like for D&D abilities
#' roll_dice_formula(dice_formula = "4d6pl1")
#' 
#' # reroll 1s until they are gone, reroll 2s once
#' roll_dice_formula(dice_formula = "3d6rr1ro2")
#' 
#' # minimum of 2 on each dice
#' roll_dice_formula(dice_formula = "4d6mi2")
#' 
#' # more than one group of dice
#' roll_dice_formula(dice_formula = "1d8+1d6+2")
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
  
  # Parse the formula (see dice-eval.R), then roll it
  dice_expression <- parse_dice_expression(dice_formula)
  
  result_df <- tibble::tibble(
    round = as.integer(rep(1:rounds, each = times)),
    nr    = as.integer(rep(1:times, times = rounds)),
    result = eval_dice_expression(dice_expression, n = rounds * times, prob = prob)
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
