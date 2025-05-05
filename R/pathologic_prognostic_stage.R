usethis::use_package("data.table")
#' Calculate Pathologic Prognostic Stage from Surgical Pathology Results
#'
#' @param t Clinical T (tumor) stage between 0 and 4, or "is" (in-situ)
#' @param n Clinical N (nodal) stage between 0 and 3
#' @param m Clinical M (metastatic) stage, either TRUE or FALSE
#' @param er Clinical ER (estrogen receptor) status, either TRUE or FALSE
#' @param pr Clinical PR (progesterone receptor) status, either TRUE or FALSE
#' @param her2 Clinical HER2 (herceptin receptor) status, either TRUE or FALSE
#' @param grade Clinical Grade between 1 and 3
#' @param t_mi Clinical microinvasive tumor, either TRUE or FALSE. Only ever
#' possibly TRUE when tumor stage is T1
#' @param n_mi Clinical micrometastatic nodal disease, either TRUE or FALSE.
#' Only ever possibly TRUE when nodal stage is N1
#' @param mgps Multigene Panel Score predicting risk of recurrence and likely
#' benefit from chemotherapy. Presumes the score is calculated with Oncotype DX.
#' Cancers that are T1-2, N0, ER+, Her2- tumors are downstaged to IA when
#' mgps < 11. If mgps is NA or no value is supplied, the prognostic stage is
#' calculated without it.
#'
#' @returns a character string representing pathologic prognostic stage based on
#' the above variables
#' @export
#'
#' @examples pathologic_prognostic_stage(1,0,0,0,0,0,2)
pathologic_prognostic_stage <- function(t, n, m, er, pr, her2, grade, t_mi = F, n_mi = F, mgps = NA){

  #multigene panel test score
  if(is.numeric(as.numeric(mgps)) & !is.nan(mgps)){
    if(as.numeric(t) %in% c(1,2) &
       as.numeric(n) %in% c(0) &
       as.numeric(m) == 0 &
       as.logical(as.numeric(er)) &
       !as.logical(as.numeric(her2)) &
       as.numeric(mgps) < 11){
      return("IA")
    }
  }
  # metastatic disease
  if(as.numeric(m) == 1) {
    return('IV')
  }

  result <- with(staging_lookup, Pathologic_Prognostic_Stage[
    T_int == t &
      N_int == n &
      M_int == as.numeric(m) &
      Grade_int == grade &
      ER == as.numeric(er) &
      PR == as.numeric(pr) &
      Her2 == as.numeric(her2) &
      T_mi == t_mi &
      N_mi == n_mi
  ])

  if(rlang::is_empty(result)){
    return(NA)
  } else {
    return(result)
  }
}
