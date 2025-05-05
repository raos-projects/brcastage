usethis::use_package("data.table")
#' Calculate Anatomic Stage from TNM Measures on imaging/biopsy
#'
#' @param t Clinical T (tumor) stage between 0 and 4
#' @param n Clinical N (nodal) stage between 0 and 3
#' @param m Clinical M (metastatic) stage, either TRUE or FALSE
#' @param t_mi Clinical microinvasive tumor, either TRUE or FALSE. Only ever
#' possibly TRUE when tumor stage is T1
#' @param n_mi Clinical micrometastatic nodal disease, either TRUE or FALSE.
#' Only ever possibly TRUE when nodal stage is N1
#'
#' @returns a character string representing clinical prognostic stage based on
#' the above variables
#' @export
#'
#' @examples anatomic_stage(1,0,0)
anatomic_stage <- function(t, n, m, t_mi = F, n_mi = F){

  #metastatic disease
  if(as.numeric(m) == 1) {
    return('IV')
  }

  result <- with(staging_lookup, Anatomic_Stage[
    T_int == t &
      N_int == n &
      M_int == m &
      T_mi == t_mi &
      N_mi == n_mi
  ])

  if(rlang::is_empty(result)){
    return(NA)
  } else {
    return(result[[1]])
  }
}
