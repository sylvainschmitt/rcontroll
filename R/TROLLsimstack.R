#' @importFrom sp SpatialPixelsDataFrame
NULL

#'An S4 class to represent TROLL outputs
#'
#'This is an S4 class to represent TROLL outputs.
#'
#'@slot name char. model name
#'@slot path char. path to the model
#'@slot abundances list. abundances data frames
#'@slot agb df. agb data frame
#'@slot ba list. ba data frames
#'@slot dbh df. dbh data frame
#'@slot death list. death data frames
#'@slot disturbance df. tree killed in disturbance data frame
#'@slot final_pattern df. final pattern data frame
#'@slot gpp df. gpp data frame
#'@slot info list. model info
#'@slot litterfall df. litterfall data frame
#'@slot npp df. npp data frame
#'@slot par df. par data frame
#'@slot paramspace list. model space parameters
#'@slot ppfd0 df. ground level ppfd data frames
#'@slot R list. respiration data frames
# @slot site list. site data frames
#'@slot sp_par df. species par data frame
#'@slot vertd df. vertd data.frame
#'
#'@export
setClass('TROLLsimstack',
         representation(
           name = 'character',
           path = 'character',
           abundances = 'list',
           agb = 'data.frame',
           ba = 'list',
           # cica = 'data.frame', To massive
           dbh = 'data.frame',
           death = 'list',
           disturbance = 'data.frame',
           final_pattern = 'SpatialPixelsDataFrame',
           gpp = 'data.frame',
           info = 'list',
           # leafdens = 'list', To massive
           litterfall = 'data.frame',
           npp = 'data.frame',
           par = 'list',
           paramspace = 'list',
           ppfd0 = 'data.frame',
           R = 'list',
           # site = 'list',
           sp_par = 'data.frame',
           vertd = 'data.frame'
         ),
         prototype(
           name = character(),
           path = character(),
           abundances = list(),
           agb = data.frame(),
           ba = list(),
           dbh = data.frame(),
           death = list(),
           disturbance = data.frame(),
           final_pattern = new('SpatialPixelsDataFrame'),
           gpp = data.frame(),
           info = list(),
           litterfall = data.frame(),
           npp = data.frame(),
           par = list(),
           paramspace = list(),
           ppfd0 = data.frame(),
           R = list(),
           # site = list(),
           sp_par = data.frame(),
           vertd =  data.frame()
         )
)

TROLLsimstack <- function(
  name = character(),
  path = character(),
  abundances = list(),
  agb = data.frame(),
  ba = list(),
  dbh = data.frame(),
  death = list(),
  disturbance = data.frame(),
  final_pattern = new('SpatialPixelsDataFrame'),
  gpp = data.frame(),
  info = list(),
  litterfall = data.frame(),
  npp = data.frame(),
  par = list(),
  paramspace = list(),
  ppfd0 = data.frame(),
  R = list(),
  # site = list(),
  sp_par = data.frame(),
  vertd = data.frame()
){
  return(new('TROLLsim',
             name = name,
             path = path,
             abundances = abundances,
             agb = agb,
             ba = ba,
             dbh = dbh,
             death = death,
             disturbance = disturbance,
             final_pattern = final_pattern,
             gpp = gpp,
             info = info,
             litterfall = litterfall,
             npp = npp,
             par = par,
             paramspace = paramspace,
             ppfd0 = ppfd0,
             R = R,
             # site = site,
             sp_par = sp_par,
             vertd = vertd
  )
  )
}
