#****************************************************************
#
#  Functions for Updating via DropBox or Local file systen
#
#****************************************************************

#' getTreeMapCSV
#'
#' reads the csv file for the treemap from dropbox
#' @param DropBox_dir the directory on dropbox for the organization
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @param tabset path to results tabs json file
#' @param data_treemap filename to use for the csv data
#' @export
#' @examples
#' getTreeMapCSV(DropBox_dir,budget=budget,data_treemap="/data_treemap.csv")


getTreeMapCSV<-function(DropBox_dir,local_dir,budget=input$budget,tabset=input$tabset,data_treemap="/data_treemap.csv",use_DropBox=T){

  if(use_DropBox==T){
    df<-drop_read_csv(paste0(DropBox_dir,budget,data_treemap))
    colnames(df)[5]<-'#'
  }
  if(use_DropBox==F){
    df<-read.csv(paste0(local_dir,budget,data_treemap),header=T)
    colnames(df)[5]<-'#'
  }

  return(df)

}


#' makeTreeMapData
#'
#' reads the csv file for the treemap from dropbox and converts to JSON
#' @param df the csv file to convert to json for the tree map
#' @param Tabdata the json tab data we send along with tree map data to update the tab plus tree map widget
#' @export
#' @examples
#' getTreeMapData(DropBox_dir,budget=budget,data_treemap="/data_treemap.csv")


makeTreeMapData<-function(df,Tabdata,TabIndex=0){

  Treedata<-list()
  for(i in 1:nrow(df)){

      Treedata[[i]]<-list()
      for(j in 1:length(colnames(df))){
        Treedata[[i]][colnames(df)[j]]<-iconv(df[i,j],to="utf-8")
      }

  }

  data<-list()
  data$TabData<-Tabdata
  data$TreeData<-Treedata
  data$TabIndex<-TabIndex


  return(data)

}


#' getSummaryAll
#'
#' sends the tab data and treemap data to the client as well as updates the summary all data for program summary
#' @param session shiny server session variable
#' @param DropBox_dir the directory on dropbox for the organization
#' @param tab what tabset to use from our radio buttons, the config folder should have the tab files in json format ending in "_tabs.json"
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @param values reactiveValue that stores the summary data
#' @export
#' @examples
#' getSummaryAll(DropBox_dir,local_dir,budget=input$budget_year,tab=input$tabset,use_DropBox=T)

getSummaryAll<-function(DropBox_dir,local_dir,budget=input$budget_year,tab=input$tabset,use_DropBox=T){

      #Get the summaryall data for this budget - we use this for program details at the bottom level
      if(use_DropBox==T){

          filepath<-file.path(tempdir(),'summaryall.xlsx')
          drop_download(path=paste0(DropBox_dir,budget,'/summaryall.xlsx'),local_path=filepath,overwrite=T)

          SummaryAll<-read.xlsx(filepath, sheet = 1, startRow = 1, colNames = TRUE,
                                     rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                                     rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
          SummaryAll$Program<-gsub(":"," - ",SummaryAll$Program)
          SummaryAll$Program<-gsub("!",".",SummaryAll$Program)

          #remove the files!
          if(file.exists(filepath))(file.remove(filepath))
      }

       if(use_DropBox==F){

          filepath<-file.path(local_dir,budget,'summaryall.xlsx')

          SummaryAll<-read.xlsx(filepath, sheet = 1, startRow = 1, colNames = TRUE,
                                     rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                                     rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
          SummaryAll$Program<-gsub(":"," - ",SummaryAll$Program)
          SummaryAll$Program<-gsub("!",".",SummaryAll$Program)

      }


      return(SummaryAll)

}

#' updateTreeMap_Tab_SummaryAll
#'
#' sends the tab data and treemap data to the client as well as updates the summary all data for program summary
#' @param session shiny server session variable
#' @param DropBox_dir the directory on dropbox for the organization
#' @param local_dir the local directory where budgets are kept
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @param tab what tabset to use from our radio buttons, the config folder should have the tab files in json format ending in "_tabs.json"
#' @param values reactiveValue that stores the summary data
#' @export
#' @examples
#' getSummaryAll(DropBox_dir,local_dir,budget=input$budget_year,tab=input$tabset,use_DropBox=T)

updateTreeMap_Tab_SummaryAll<-function(session,DropBox_dir,local_dir,budget,tabset,TabIndex,data_treemap="/data_treemap.csv",use_DropBox=F){

         #Get the CSV and Tab data and update the plot
        TreeMapCSV<-getTreeMapCSV(DropBox_dir,local_dir,budget,tabset,data_treemap="/data_treemap.csv",use_DropBox)

        TabsJSON<-getTabData(DropBox_dir,local_dir,tab=tabset,budget,use_DropBox)

        #Send both tree data and tabset data to the client
        data<-makeTreeMapData(df=TreeMapCSV,Tabdata=TabsJSON,TabIndex=TabIndex)
        data<-jsonlite::toJSON(data)
        session$sendCustomMessage(type="updatebudget",data)


      #Get the Summary Data
        SummaryAll<-getSummaryAll(DropBox_dir,local_dir,budget,tabset,use_DropBox)

        Budget_Tab<-paste0(budget,"_",tabset)

        values<-list()
        values$TreeMapCSV<-TreeMapCSV
        values$TabsJSON<-TabsJSON
        values$SummaryAll<-SummaryAll
        values$Budget_Tab<-Budget_Tab

        return(values)

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
              report.about<-HTML(readLines(paste0('./budgets/',budget,'/reports/reports_about.txt'),warn=F))
             }

            insertTab(inputId = "chart_tabs",position='after',target="Table",
                 tab=tabPanel('Reports',style='height:700px;margin-left:15px;',

                      br(),
                      fluidRow(column(4,selectInput('presentReport_select','Select Available Reports for this Budget',choices=reports,width='100%')),
                               column(2,tags$div(style='margin-top:25px'),downloadButton('presentReport_download',label='Download'))),
                      report.about
                 )
            )
          }else{

            removeTab("chart_tabs", target='Reports')
          }


}





