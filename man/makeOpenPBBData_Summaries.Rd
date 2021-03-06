% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakeSummaries.R
\name{makeOpenPBBData_Summaries}
\alias{makeOpenPBBData_Summaries}
\title{makeOpenPBBData_Summaries}
\usage{
makeOpenPBBData_Summaries(db_name_new, db_host_new, db_user, db_pw,
  BudgetID, CostModelID, ScoreReview = "ScorePeer")
}
\arguments{
\item{db_name_new}{name of org database}

\item{db_host_new}{database host}

\item{db_user}{database user}

\item{db_pw}{database pw}

\item{BudgetID}{Budget ID we want to summarize}

\item{CostModelID}{Costmodel ID of the PBB accounting structure}
}
\description{
makes two summary files outputed as a list. csv is the treemap summary. summaryall is the data for the program level summary
}
\examples{
makeOpenPBBData_Summaries(db_name_new,db_host_new,BudgetID,CostModelID)
}
