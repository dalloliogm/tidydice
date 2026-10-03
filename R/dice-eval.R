#  Parser and evaluator for dice expressions, following the syntax of Avrae's
#  d20 library (https://github.com/avrae/d20).
#
#  Grammar (whitespace is ignored, case is ignored):
#
#    expr     := term (("+" | "-") term)*
#    term     := power (("*" | "/") power)*
#    power    := unary (("^" | "**") power)?
#    unary    := ("-" | "+") unary | atom
#    atom     := "(" expr ")" | dice | number
#    dice     := [count] "d" (sides | "%") modifier*
#    modifier := ("k" | "p" | "rr" | "ro" | "ra" | "e" | "mi" | "ma") selector+
#    selector := ["h" | "l" | "<" | ">"] number
#
#  Consecutive identical modifiers are merged (e5e6 is e(5,6)), as in d20.
#
#  A parsed expression is a tree of lists with a `type` of "num", "neg",
#  "binop" or "dice".

# Maximum number of dice rolled for one set of dice (same as d20)
MAX_DICE_ROLLED <- 1000

# --- Parser ------------------------------------------------------------------

#' Parse a dice expression
#'
#' @param dice_formula A string containing a dice formula, e.g. 4d6e6kh3+2
#' @param require_dice If TRUE, the formula must contain at least one dice
#' @param validate If TRUE, check that the modifiers can be rolled (e.g. no 
#'   reroll of a number that is not on the dice). Otherwise only the syntax is checked.
#' @return A tree (nested lists) describing the expression. Number and dice 
#'   nodes also keep their source text, for parse_dice_formula()

parse_dice_expression <- function(dice_formula, require_dice = TRUE, validate = TRUE) {

  st <- new.env()
  st$orig <- gsub("\\s", "", dice_formula) # same length as st$s: keeps the case
  st$s <- tolower(st$orig)
  st$pos <- 1L
  st$n_dice <- 0L

  fail <- function(msg) {
    stop(msg, call. = FALSE)
  }

  # Consume `pattern` at the current position, return the capture groups or NULL
  take <- function(pattern) {
    rest <- substring(st$s, st$pos)
    m <- regmatches(rest, regexec(paste0("^(?:", pattern, ")"), rest, perl = TRUE))[[1]]
    if (length(m) == 0) {
      return(NULL)
    }
    st$pos <- st$pos + nchar(m[1])
    m
  }

  parse_expr <- function() {
    node <- parse_term()
    while (!is.null(op <- take("[+-]"))) {
      node <- list(type = "binop", op = op[1], op_text = op[1], lhs = node, rhs = parse_term())
    }
    node
  }

  parse_term <- function() {
    node <- parse_power()
    while (!is.null(op <- take("\\*(?!\\*)|/"))) {
      node <- list(type = "binop", op = op[1], op_text = op[1], lhs = node, rhs = parse_power())
    }
    node
  }

  parse_power <- function() {
    node <- parse_unary()
    if (!is.null(op <- take("\\^|\\*\\*"))) {
      node <- list(type = "binop", op = "^", op_text = op[1], lhs = node, 
                   rhs = parse_power())
    }
    node
  }

  parse_unary <- function() {
    if (!is.null(op <- take("[+-]"))) {
      node <- parse_unary()
      if (op[1] == "-") {
        node <- list(type = "neg", x = node)
      }
      return(node)
    }
    parse_atom()
  }

  parse_atom <- function() {
    start <- st$pos
    source_text <- function() substring(st$orig, start, st$pos - 1)
    if (!is.null(take("\\("))) {
      node <- parse_expr()
      if (is.null(take("\\)"))) {
        fail("invalid dice_formula, missing closing parenthesis")
      }
      return(node)
    }
    if (!is.null(m <- take("(\\d*)d(\\d+|%)"))) {
      node <- parse_dice(m[2], m[3])
      node$base_text <- source_text() # before the modifiers
      return(parse_dice_modifiers(node, start))
    }
    if (!is.null(m <- take("\\d+(?:\\.\\d+)?"))) {
      return(list(type = "num", value = as.numeric(m[1]), text = source_text()))
    }
    fail(paste0("invalid dice_formula, cannot parse '", substring(st$s, st$pos), "'"))
  }

  parse_dice <- function(count, sides) {
    count <- if (count == "") 1 else as.numeric(count)
    sides <- if (sides == "%") 100 else as.numeric(sides)
    assertthat::assert_that(count >= 1, msg = "cannot roll 0 dice!")
    assertthat::assert_that(sides >= 1, msg = "cannot roll a d0!")
    assertthat::assert_that(count <= MAX_DICE_ROLLED, msg = "Too many dice rolled.")
    st$n_dice <- st$n_dice + 1L
    list(type = "dice", count = count, sides = sides, mods = list(), raw_mods = list())
  }

  # Add the modifiers that follow the dice
  parse_dice_modifiers <- function(node, start) {
    count <- node$count
    sides <- node$sides
    
    # Like d20, consecutive identical modifiers are merged: e5e6 is e(5,6)
    mods <- list()
    raw_mods <- list() # as written, for parse_dice_formula()
    while (!is.null(m <- take("rr|ro|ra|mi|ma|k|p|e"))) {
      op <- m[1]
      sels <- parse_selectors(op)
      raw_mods <- c(raw_mods, lapply(sels, function(sel) {
        list(op = op, cat = sel$cat, num = sel$num)
      }))
      last <- length(mods)
      if (last > 0 && mods[[last]]$op == op && !op %in% c("mi", "ma")) {
        mods[[last]]$sels <- c(mods[[last]]$sels, sels)
      } else {
        mods[[last + 1]] <- list(op = op, sels = sels)
      }
    }
    
    can_grow <- FALSE # can the set contain more dice than `count`?
    for (mod in mods) {
      if (validate) validate_modifier(mod, count, sides, can_grow)
      if (mod$op %in% c("e", "ra")) {
        can_grow <- TRUE
      }
    }
    node$mods <- mods
    node$raw_mods <- raw_mods
    node$text <- substring(st$orig, start, st$pos - 1)
    node
  }

  parse_selectors <- function(op) {
    sels <- list()
    while (!is.null(m <- take("([hl<>]?)(\\d+)"))) {
      sels[[length(sels) + 1]] <- list(cat = m[2], num = as.numeric(m[3]))
    }
    if (length(sels) == 0) {
      fail(paste0("invalid dice_formula, modifier '", op, "' needs a selector, e.g. ",
                  op, "6"))
    }
    sels
  }

  node <- parse_expr()
  if (st$pos <= nchar(st$s)) {
    fail(paste0("invalid dice_formula, cannot parse '", substring(st$s, st$pos), "'"))
  }
  if (require_dice) {
    assertthat::assert_that(st$n_dice > 0,
                            msg = "dice_formula need to contain at least one d statement")
  }
  node
}

# Does a selector match a die with value `x`? Only for selectors that do not
# depend on the other dice of the set (not h/l).
selector_matches <- function(sel, x) {
  switch(sel$cat,
         "<" = x < sel$num,
         ">" = x > sel$num,
         x == sel$num)
}

# Check that a modifier makes sense for a die with `sides` sides
validate_modifier <- function(mod, count, sides, can_grow) {
  op <- mod$op

  if (op %in% c("mi", "ma")) {
    assertthat::assert_that(
      length(mod$sels) == 1 && mod$sels[[1]]$cat == "",
      msg = paste0("invalid dice_formula, '", op, "' needs a plain number, e.g. ", op, "2"))
  }

  if (op == "k") {
    for (sel in mod$sels) {
      if (sel$cat %in% c("h", "l")) {
        assertthat::assert_that(sel$num > 0,
                                msg = "invalid kh/kl formula, can't keep less than 1 die")
        assertthat::assert_that(can_grow || sel$num <= count,
                                msg = "invalid kh/kl formula, can't keep more dice than rolled")
      }
    }
  }

  if (op %in% c("rr", "ro", "ra", "e")) {
    # selectors that do not depend on the other dice: can be checked now
    fixed <- Filter(function(sel) !sel$cat %in% c("h", "l"), mod$sels)
    if (length(fixed) == length(mod$sels)) {
      sides_hit <- Reduce(`|`, lapply(fixed, selector_matches, x = seq_len(sides)))
      what <- if (op == "e") "exploding dice" else "reroll"
      assertthat::assert_that(
        any(sides_hit),
        msg = paste0("invalid ", what, " specification, no side of the dice matches"))
      if (op %in% c("e", "rr")) {
        assertthat::assert_that(
          !all(sides_hit),
          msg = paste0("invalid ", what, " specification, every side would ",
                       if (op == "e") "explode" else "be rerolled"))
      }
    }
  }
  invisible(TRUE)
}

# --- Evaluator ---------------------------------------------------------------

#' Evaluate a parsed dice expression
#'
#' @param node A tree returned by parse_dice_expression()
#' @param n Number of times the expression is evaluated
#' @param prob Vector of probabilities for each side of the dice (or NULL)
#' @return Numeric vector of length n

eval_dice_expression <- function(node, n, prob = NULL) {
  switch(node$type,
    num = rep(node$value, n),
    neg = -eval_dice_expression(node$x, n, prob),
    binop = {
      lhs <- eval_dice_expression(node$lhs, n, prob)
      rhs <- eval_dice_expression(node$rhs, n, prob)
      switch(node$op, "+" = lhs + rhs, "-" = lhs - rhs, "*" = lhs * rhs,
             "/" = lhs / rhs, "^" = lhs ^ rhs)
    },
    dice = eval_dice_group(node, n, prob)
  )
}

# Roll a group of dice (e.g. 4d6e6kh3) n times, return the n totals
eval_dice_group <- function(node, n, prob) {

  roll <- function(k) {
    sample(x = seq_len(node$sides), size = k, replace = TRUE, prob = prob)
  }

  # without modifiers all dice are summed: no need to loop
  if (length(node$mods) == 0) {
    if (node$count == 1) {
      return(as.numeric(roll(n)))
    }
    return(rowSums(matrix(roll(n * node$count), nrow = n, ncol = node$count)))
  }

  vapply(seq_len(n), function(i) {
    roll_dice_set(node$count, node$mods, roll)
  }, numeric(1))
}

# Select dice of a set. A selector is either relative to the other kept dice
# (h = highest n, l = lowest n) or compares the value (<, >, ==).
# Like in d20, only kept dice can be selected.
select_dice <- function(sels, values, kept) {
  idx <- which(kept)
  selected <- integer(0)
  for (sel in sels) {
    selected <- union(selected, switch(sel$cat,
      h = utils::head(idx[order(-values[idx], idx)], sel$num),
      l = utils::head(idx[order(values[idx], idx)], sel$num),
      idx[selector_matches(sel, values[idx])]))
  }
  selected
}

# Roll one set of dice and apply the modifiers, in the order they are written
roll_dice_set <- function(count, mods, roll) {

  values <- roll(count)
  kept <- rep(TRUE, count)
  exploded <- rep(FALSE, count)
  rolled <- count

  rolled_more <- function(k) {
    rolled <<- rolled + k
    if (rolled > MAX_DICE_ROLLED) {
      stop("Too many dice rolled.", call. = FALSE)
    }
  }
  add_dice <- function(new) {
    values <<- c(values, new)
    kept <<- c(kept, rep(TRUE, length(new)))
    exploded <<- c(exploded, rep(FALSE, length(new)))
  }

  for (mod in mods) {
    sels <- mod$sels
    switch(mod$op,
      k = {
        drop <- setdiff(which(kept), select_dice(sels, values, kept))
        kept[drop] <- FALSE
      },
      p = {
        kept[select_dice(sels, values, kept)] <- FALSE
      },
      rr = {
        # reroll until no die matches the selector
        while (length(sel <- select_dice(sels, values, kept)) > 0) {
          rolled_more(length(sel))
          values[sel] <- roll(length(sel))
        }
      },
      ro = {
        sel <- select_dice(sels, values, kept)
        rolled_more(length(sel))
        values[sel] <- roll(length(sel))
      },
      ra = {
        # like d20: at most one die is rolled again, and the new die is added
        sel <- utils::head(select_dice(sels, values, kept), 1)
        if (length(sel) > 0) {
          rolled_more(1)
          exploded[sel] <- TRUE
          add_dice(roll(1))
        }
      },
      e = {
        # new dice can explode too, but a die explodes only once
        repeat {
          sel <- setdiff(select_dice(sels, values, kept), which(exploded))
          if (length(sel) == 0) break
          rolled_more(length(sel))
          exploded[sel] <- TRUE
          add_dice(roll(length(sel)))
        }
      },
      mi = {
        values[kept & values < sels[[1]]$num] <- sels[[1]]$num
      },
      ma = {
        values[kept & values > sels[[1]]$num] <- sels[[1]]$num
      }
    )
  }

  sum(values[kept])
}
