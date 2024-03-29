% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ABM_funcs.R
\name{t_mtosev}
\alias{t_mtosev}
\title{Symptom onset to hospitalization}
\usage{
t_mtosev(n_Imh, shape.imh = 3, scale.imh = 1)
}
\arguments{
\item{n_Imh}{number of draws}

\item{shape.imh}{shape of gamma distn}

\item{scale.imh}{scale of gamma distn}
}
\value{
numeric of time spent mildly symptomatic before hospitalization (Imh)
}
\description{
Draw from distribution of proposed time spent with minor symptoms before being hospitalized for severe infections (i.e. those with B(1,p.sevsymp==1)). Used in `next.state`
}
