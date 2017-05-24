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
#'@slot full_final df. full final pattern data frame (save TROLL sim)
#'@slot gpp df. gpp data frame
#'@slot info list. model info
#'@slot litterfall df. litterfall data frame
#'@slot npp df. npp data frame
#'@slot par df. par data frame
#'@slot paramspace list. model space parameters
#'@slot paramsylviculture list. model sylviculture parameters
#'@slot ppfd0 df. ground level ppfd data frames
#'@slot R list. respiration data frames
# @slot site list. site data frames
#'@slot sp_par df. species par data frame
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
           dbh = 'data.frame',
           death = 'list',
           disturbance = 'data.frame',
           final_pattern = 'SpatialPixelsDataFrame',
           full_final = 'data.frame',
           gpp = 'data.frame',
           info = 'list',
           litterfall = 'data.frame',
           npp = 'data.frame',
           par = 'list',
           paramspace = 'list',
           paramsylviculture = 'list',
           ppfd0 = 'data.frame',
           R = 'list',
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
           full_final = data.frame(),
           gpp = data.frame(),
           info = list(),
           litterfall = data.frame(),
           npp = data.frame(),
           par = list(),
           paramspace = list(),
           paramsylviculture = list(),
           ppfd0 = data.frame(),
           R = list(),
           sp_par = data.frame(),
           vertd =  data.frame()
         )
)

TROLLsim <- function(
  name = character(),
  path = character(),
  abundances = list(),
  agb = data.frame(),
  ba = list(),
  dbh = data.frame(),
  death = list(),
  disturbance = data.frame(),
  final_pattern = new('SpatialPixelsDataFrame'),
  full_final = data.frame(),
  gpp = data.frame(),
  info = list(),
  litterfall = data.frame(),
  npp = data.frame(),
  par = list(),
  paramspace = list(),
  paramsylviculture = list(),
  ppfd0 = data.frame(),
  R = list(),
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
             full_final = full_final,
             gpp = gpp,
             info = info,
             litterfall = litterfall,
             npp = npp,
             par = par,
             paramspace = paramspace,
             paramsylviculture = paramsylviculture,
             ppfd0 = ppfd0,
             R = R,
             sp_par = sp_par,
             vertd = vertd
  )
  )
}
