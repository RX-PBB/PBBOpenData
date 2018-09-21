#**************************************************
#
#  Functions for Updating via DropBox
#
#**************************************************


#' getTreeMapData
#'
#' reads the csv file for the treemap from dropbox
#' @param DropBox_dir the directory on dropbox for the organization
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @param tabset path to results tabs json file
#' @param data_treemap filename to use for the csv data
#' @export
#' @examples
#' getTreeMapData(DropBox_dir,budget=budget,data_treemap="/data_treemap.csv")


getTreeMapData<-function(DropBox_dir,budget=input$budget,tabset=input$tabset,data_treemap="/data_treemap.csv"){

  df<-drop_read_csv(paste0(DropBox_dir,budget,data_treemap))
  colnames(df)[5]<-'#'

  data<-list()
  for(i in 1:nrow(df)){

      data[[i]]<-list()
      for(j in 1:length(colnames(df))){
        data[[i]][colnames(df)[j]]<-iconv(df[i,j],to="utf-8")
      }

  }

  #data<-jsonlite::toJSON(data)

  return(data)

}


#' getTabData
#'
#' reads the json file for the tabset of interest
#' @param DropBox_dir the directory on dropbox for the organization
#' @param tab what tabset to use from our radio buttons, the config folder should have the tab files in json format ending in "_tabs.json"
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @export
#' @examples
#' getTabData(DropBox_dir,tab=tab,budget=budget)


getTabData<-function(DropBox_dir,tab=input$tabset,budget=input$budget_year){

  filepath<-file.path(tempdir(),'tabs.json')
  drop_download(path=paste0(DropBox_dir,budget,'/config/',tab,"_tabs.json"),local_path=filepath,overwrite=T)
  json<-jsonlite::fromJSON(filepath)
  #data<-jsonlite::toJSON(data)
  if(file.exists(filepath))(file.remove(filepath))

  return(json)

}

#' updateDropBoxBudget
#'
#' sends the tab data and treemap data to the client as well as updates the summary all data for program summary
#' @param DropBox_dir the directory on dropbox for the organization
#' @param tab what tabset to use from our radio buttons, the config folder should have the tab files in json format ending in "_tabs.json"
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @export
#' @examples
#' updateDropBoxBudget(budget=input$budget_year,tab=input$tabset,DropBox_dir)

updateDropBoxBudget<-function(session,DropBox_dir,budget=input$budget_year,tab=input$tabset){

      #Get the tree data
      Treedata<-getTreeMapData(DropBox_dir,budget=budget,data_treemap="/data_treemap.csv")

      #Get the tab data
      Tabdata<-getTabData(DropBox_dir,tab=tab,budget=budget)

      data<-list()
      data$TabData<-Tabdata
      data$TreeData<-Treedata

      #Send both tree data and tabset data to the client
      data<-jsonlite::toJSON(data)
      session$sendCustomMessage(type="updatebudget",data)

      #Get the summaryall data for this budget - we use this for program details at the bottom level
      filepath<-file.path(tempdir(),'summaryall.xlsx')
      drop_download(path=paste0(DropBox_dir,budget,'/summaryall.xlsx'),local_path=filepath,overwrite=T)

      SummaryAll<-read.xlsx(filepath, sheet = 1, startRow = 1, colNames = TRUE,
                                 rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                                 rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
      SummaryAll$Program<-gsub(":"," - ",SummaryAll$Program)
      SummaryAll$Program<-gsub("!",".",SummaryAll$Program)

      values$SummaryAll<-SummaryAll

      #remove the files!
      if(file.exists(filepath))(file.remove(filepath))

}

