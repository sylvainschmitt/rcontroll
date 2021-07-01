#'An S4 class to represent TROLL outputs
#'
#'This is an S4 class to represent TROLL outputs.
#'
#'@slot name char. model name
#'@slot path char. path to the model
#'@slot abundances list. abundances data frames
#'@slot agb df. agb data frame
#'@slot ba list. ba data frames
#'@slot death list. death data frames
#'@slot final_pattern df. final pattern data frame
#'@slot gpp df. gpp data frame
#'@slot info chr. model info
#'@slot litterfall df. litterfall data frame
#'@slot npp df. npp data frame
#'@slot inputs list. model inputs (general, species, climate, daily, soil)
#'@slot ppfd0 df. ground level ppfd data frames
#'@slot R list. respiration data frames
# @slot site list. site data frames
#'@slot vertd df. vertd data.frame
#'
#'@export
setClass('TROLLsim',
         representation(
           name = 'character',
           path = 'character',
           abundances = 'list',
           agb = 'data.frame',
           ba = 'list',
           death = 'list',
           final_pattern = 'data.frame',
           gpp = 'data.frame',
           info = 'character',
           litterfall = 'data.frame',
           npp = 'data.frame',
           inputs = 'list',
           ppfd0 = 'data.frame',
           R = 'list',
           vertd = 'data.frame'
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
           litterfall = data.frame(),
           npp = data.frame(),
           inputs = list(),
           ppfd0 = data.frame(),
           R = list(),
           vertd =  data.frame()
         )
)

TROLLsim <- function(
  name = character(),
  path = character(),
  abundances = list(),
  agb = data.frame(),
  ba = list(),
  death = list(),
  final_pattern = data.frame(),
  gpp = data.frame(),
  info = character(),
  litterfall = data.frame(),
  npp = data.frame(),
  inputs = list(),
  ppfd0 = data.frame(),
  R = list(),
  vertd = data.frame()
){
  return(new('TROLLsim',
             name = name,
             path = path,
             abundances = abundances,
             agb = agb,
             ba = ba,
             death = death,
             final_pattern = final_pattern,
             gpp = gpp,
             info = info,
             litterfall = litterfall,
             npp = npp,
             inputs = inputs,
             ppfd0 = ppfd0,
             R = R,
             vertd = vertd
  )
  )
}
