#' present_initialize
#'
#' initializes the dropdowns on the page and gets the summary data files
#' @param session shiny server session variable
#' @param use_DropBox essentially depricated. we moved to local file system.. Exploring moving to AWS S3 buckets
#' @param DropBox_dir the directory on dropbox for the organization
#' @param local_dir the local directory where budgets are kept
#' @export
#' @examples
#' present_initialize(session,use_DropBox=FALSE,local_dir="./budgets/",DropBox_dir=NULL)


present_initialize<-function(session,use_DropBox=FALSE,local_dir="./budgets/",DropBox_dir=NULL){

  if (use_DropBox==T){

    budgets<-drop_dir(path=DropBox_dir)$name
    tabs<-drop_dir(path=paste0(DropBox_dir,budgets[1],'/config'))$name
    #budgets<-budgets[which(!is.element(budgets,'config'))]
    #budgets<-rev(sort(budgets))

  }
  if(use_DropBox==F){
    budgets<-list.files(path=local_dir)
    budgets<-rev(budgets)
    tabs<-list.files(path=paste0(local_dir,budgets[1],'/config'))
  }


  updateSelectInput(session, "budget_year",
                    label = "Select Budget",
                    choices = budgets,
                    selected = budgets[1])



  tabs<-sort(gsub("_tabs.json","",tabs))

  if(is.element("Results",tabs))(tab_selected<-"Results")else(tab_selected<-tabs[1])

  updateSelectInput(session, "tabset",
                    label = "Select Tabset",
                    choices = tabs,
                    selected = tab_selected)



  update_values<-updateTreeMap_Tab_SummaryAll(session,DropBox_dir,local_dir,budget=budgets[1],tabset=tab_selected,TabIndex=0,data_treemap="/data_treemap.csv",use_DropBox)

  #Update the reactive values
  #values$TreeMapCSV<-update_values$TreeMapCSV
  #values$TabsJSON<-update_values$TabsJSON
  #values$SummaryAll<-update_values$SummaryAll
  #values$Budget_Tab<-update_values$Budget_Tab

  #Update Department Filter
  choices = c('All Available',sort(unique(as.character(update_values$TreeMapCSV$Department))))
  updateSelectInput(session, "department",
                    label = "Select a Service Area",
                    choices = choices,
                    selected = choices[1])


  #Check for existance of reports - if the exist update the tabs
  budget<-budgets[1] #test


  update_ReportsTab(local_dir,budget)

  return(update_values)


}

#' update_ReportsTab
#'
#' Checks for any reports and adds or removes tab to main tabset of charts
#' @param local_dir the local directory where budgets are kept
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @export
#' @examples
#' update_ReportsTab(local_dir,budget)

update_ReportsTab<-function(local_dir,budget){
  reports<-list.files(path=paste0(local_dir,budget,'/reports'))
  if(length(reports)>0){

    reports.about<-NULL
    if(is.element('reports_about.txt',reports))(reports.about<-'reports_about.txt')
    reports<-reports[which(!is.element(reports,'reports_about.txt'))]

    if(!is.null(reports.about)){
      reports.about<-HTML(readLines(paste0('./budgets/',budget,'/reports/reports_about.txt'),warn=F))
    }

    insertTab(inputId = "chart_tabs",position='after',target="Table",
              tab=tabPanel('Reports',style='height:700px;margin-left:15px;',

                           br(),
                           fluidRow(column(4,selectInput('presentReport_select','Select Available Reports for this Budget',choices=reports,width='100%')),
                                    column(2,tags$div(style='margin-top:25px'),downloadButton('presentReport_download',label='Download'))),
                           reports.about
              )
    )
  }else{

    removeTab("chart_tabs", target='Reports')
  }


}

