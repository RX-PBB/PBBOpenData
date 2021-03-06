% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LoadBudgetData.R
\name{updateTreeMap_Tab_SummaryAll}
\alias{updateTreeMap_Tab_SummaryAll}
\title{updateTreeMap_Tab_SummaryAll}
\usage{
updateTreeMap_Tab_SummaryAll(session, DropBox_dir, local_dir, budget,
  tabset, TabIndex, data_treemap = "/data_treemap.csv",
  use_DropBox = F, update_treepmap = T)
}
\arguments{
\item{session}{shiny server session variable}

\item{DropBox_dir}{the directory on dropbox for the organization}

\item{local_dir}{the local directory where budgets are kept}

\item{budget}{what budget to use, the csv file should be in a folder of the same name}

\item{tab}{what tabset to use from our radio buttons, the config folder should have the tab files in json format ending in "_tabs.json"}

\item{values}{reactiveValue that stores the summary data}
}
\description{
sends the tab data and treemap data to the client as well as updates the summary all data for program summary
}
\examples{
getSummaryAll(DropBox_dir,local_dir,budget=input$budget_year,tab=input$tabset,use_DropBox=T)
}
