usethis::use_package("data.table")
#' Calculate Clinical Prognostic Stage from Biopsy Results
#'
#' @param t Clinical T (tumor) stage between 0 and 4
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
#'
#' @returns a character string representing clinical prognostic stage based on
#' the above variables
#' @export
#'
#' @examples clinical_prognostic_stage(1,0,0,0,0,0,2)
clinical_prognostic_stage <- function(t, n, m, er, pr, her2, grade, t_mi = F, n_mi = F){

  #metastatic disease
  if(as.numeric(m) == 1) {
    return(factor('IV',levels = c('0','IA','IB','IC','IIA','IIB','IIC','IIIA','IIIB','IIIC','IV')))
  }

  result <- with(staging_lookup, Clinical_Prognostic_Stage[
    T_int == t &
      N_int == n &
      M_int == m &
      Grade_int == grade &
      ER == er &
      PR == pr &
      Her2 == her2 &
      T_mi == t_mi &
      N_mi == n_mi
  ])

  if(rlang::is_empty(result)){
    return(NA)
  } else {
    return(result)
  }
}
