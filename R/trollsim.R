#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @slot name char. model name
#' @slot path char. path to the model
#' @slot abundances list. abundances data frames
#' @slot agb df. agb data frame
#' @slot ba list. ba data frames
#' @slot death list. death data frames
#' @slot final_pattern df. final pattern data frame
#' @slot gpp df. gpp data frame
#' @slot info chr. model info
#' @slot log chr. model log
#' @slot litterfall df. litterfall data frame
#' @slot npp df. npp data frame
#' @slot inputs list. model inputs (general, species, climate, daily, soil)
#' @slot ppfd0 df. ground level ppfd data frames
#' @slot R list. respiration data frames
#' @slot vertd df. vertd data.frame
#'
#' @name trollsim
#' @export
setClass(
  "trollsim",
  representation(
    name = "character",
    path = "character",
    abundances = "list",
    agb = "data.frame",
    ba = "list",
    death = "list",
    final_pattern = "data.frame",
    gpp = "data.frame",
    info = "character",
    log = "character",
    litterfall = "data.frame",
    npp = "data.frame",
    inputs = "list",
    ppfd0 = "data.frame",
    R = "list",
    vertd = "data.frame"
  ),
  prototype(
    name = character(),
    path = character(),
    abundances = list(),
    agb = data.frame(),
    ba = list(),
    death = list(),
    final_pattern = data.frame(),
    gpp = data.frame(),
    info = character(),
    log = character(),
    litterfall = data.frame(),
    npp = data.frame(),
    inputs = list(),
    ppfd0 = data.frame(),
    R = list(),
    vertd = data.frame()
  )
)

#' @export
#' @rdname trollsim
trollsim <- function(name = character(),
                     path = character(),
                     abundances = list(),
                     agb = data.frame(),
                     ba = list(),
                     death = list(),
                     final_pattern = data.frame(),
                     gpp = data.frame(),
                     info = character(),
                     log = character(),
                     litterfall = data.frame(),
                     npp = data.frame(),
                     inputs = list(),
                     ppfd0 = data.frame(),
                     r = list(),
                     vertd = data.frame()) {
  return(new("trollsim",
    name = name,
    path = path,
    abundances = abundances,
    agb = agb,
    ba = ba,
    death = death,
    final_pattern = final_pattern,
    gpp = gpp,
    info = info,
    log = log,
    litterfall = litterfall,
    npp = npp,
    inputs = inputs,
    ppfd0 = ppfd0,
    r = r,
    vertd = vertd
  ))
}
