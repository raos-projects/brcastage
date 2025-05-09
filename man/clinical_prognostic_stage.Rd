% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clinical_prognostic_stage.R
\name{clinical_prognostic_stage}
\alias{clinical_prognostic_stage}
\title{Calculate Clinical Prognostic Stage from Biopsy Results}
\usage{
clinical_prognostic_stage(
  t,
  n,
  m,
  er,
  pr,
  her2,
  grade,
  t_mi = F,
  n_mi = F,
  mgps = NA
)
}
\arguments{
\item{t}{Clinical T (tumor) stage between 0 and 4}

\item{n}{Clinical N (nodal) stage between 0 and 3}

\item{m}{Clinical M (metastatic) stage, either TRUE or FALSE}

\item{er}{Clinical ER (estrogen receptor) status, either TRUE or FALSE}

\item{pr}{Clinical PR (progesterone receptor) status, either TRUE or FALSE}

\item{her2}{Clinical HER2 (herceptin receptor) status, either TRUE or FALSE}

\item{grade}{Clinical Grade between 1 and 3}

\item{t_mi}{Clinical microinvasive tumor, either TRUE or FALSE. Only ever
possibly TRUE when tumor stage is T1}

\item{n_mi}{Clinical micrometastatic nodal disease, either TRUE or FALSE.
Only ever possibly TRUE when nodal stage is N1}

\item{mgps}{Multigene Panel Score predicting risk of recurrence and likely
benefit from chemotherapy. Presumes the score is calculated with Oncotype DX.
Cancers that are T1-2, N0, ER+, Her2- tumors are downstaged to IA when
mgps < 11. If mgps is NA or no value is supplied, the prognostic stage is
calculated without it.}
}
\value{
a character string representing clinical prognostic stage based on
the above variables
}
\description{
Calculate Clinical Prognostic Stage from Biopsy Results
}
\examples{
clinical_prognostic_stage(1,0,F,F,F,F,2)
}
