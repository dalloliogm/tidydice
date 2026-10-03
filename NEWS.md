## tidydice (development version)

* roll_dice_formula(): exploding dice are now simulated die by die, replacing
  the geometric-distribution approximation. This fixes wrong results with
  several dice, `prob` and `kh`/`kl`.
* roll_dice_formula(): add `e>N` and `e<N` (and bare `e`) exploding syntax,
  as in Avrae.
* roll_dice_formula(): `1d2e1` is now valid; a formula where every side 
  explodes (e.g. `1d6e>1`) raises an error.

## tidydice 1.0.0 (2022-02-01)

* add roll_dice_formula()
* fix help plot_binom()
* change default color in plot_binom() to "coral"
* Update Vignette
* Update README
* Default color for success in plot_dice() & plot_coin() is "gold"

## tidydice 0.0.6 (2020-01-07)

* improve vignette
* add function binom()
* add function binom_dice()
* add function binom_coin()
* add function plot_binom()
* plot_dice() detailed = FALSE as default

## tidydice 0.0.4 (2019-11-26)

* add function plot_dice()
* add function force_dice()
* add function force_coin()
* rename and improve vignette

## tidydice 0.0.3 (2019-11-15)

* roll_dice()
* flip_coin()