#**************************************************
#
#  Functions for Updating via DropBox
#
#**************************************************

#' getTreeMapData_CSV
#'
#' reads the csv file for the treemap from dropbox
#' @param DropBox_dir the directory on dropbox for the organization
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @param tabset path to results tabs json file
#' @param data_treemap filename to use for the csv data
#' @export
#' @examples
#' getTreeMapData_CSV(DropBox_dir,budget=budget,data_treemap="/data_treemap.csv")


getTreeMapData_CSV<-function(DropBox_dir,local_dir,budget=input$budget,tabset=input$tabset,data_treemap="/data_treemap.csv",use_DropBox=T){

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


#' getTreeMapData
#'
#' reads the csv file for the treemap from dropbox and converts to JSON
#' @param DropBox_dir the directory on dropbox for the organization
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @param tabset path to results tabs json file
#' @param data_treemap filename to use for the csv data
#' @export
#' @examples
#' getTreeMapData(DropBox_dir,budget=budget,data_treemap="/data_treemap.csv")


getTreeMapData<-function(DropBox_dir,local_dir,budget=input$budget,tabset=input$tabset,data_treemap="/data_treemap.csv",use_DropBox=T){

  # if(use_DropBox==T){
  #   df<-drop_read_csv(paste0(DropBox_dir,budget,data_treemap))
  #   colnames(df)[5]<-'#'
  # }
  # if(use_DropBox==F){
  #   df<-read.csv(paste0(local_dir,budget,data_treemap),header=T)
  #   colnames(df)[5]<-'#'
  # }
  
  df<-getTreeMapData_CSV(DropBox_dir,local_dir,budget=budget,tabset=tabset,data_treemap,use_DropBox=use_DropBox)

    
  data<-list()
  for(i in 1:nrow(df)){

      data[[i]]<-list()
      for(j in 1:length(colnames(df))){
        data[[i]][colnames(df)[j]]<-iconv(df[i,j],to="utf-8")
      }

  }

  

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


getTabData<-function(DropBox_dir,local_dir,tab=input$tabset,budget=input$budget_year,use_DropBox=T){

  if(use_DropBox==T){

      filepath<-file.path(tempdir(),'tabs.json')
      drop_download(path=paste0(DropBox_dir,budget,'/config/',tab,"_tabs.json"),local_path=filepath,overwrite=T)
      json<-jsonlite::fromJSON(filepath)
      if(file.exists(filepath))(file.remove(filepath))

  }

  if(use_DropBox==F){

      json<-jsonlite::fromJSON(paste0(local_dir,budget,"/config/",tab,"_tabs.json"))

  }
  #data<-jsonlite::toJSON(data)


  return(json)

}

#' updateDropBoxBudget
#'
#' sends the tab data and treemap data to the client as well as updates the summary all data for program summary
#' @param session shiny server session variable
#' @param DropBox_dir the directory on dropbox for the organization
#' @param tab what tabset to use from our radio buttons, the config folder should have the tab files in json format ending in "_tabs.json"
#' @param budget what budget to use, the csv file should be in a folder of the same name
#' @param values reactiveValue that stores the summary data
#' @export
#' @examples
#' updateDropBoxBudget(session,DropBox_dir,budget=input$budget_year,tab=input$tabset,values=values$SummaryAll)

updateDropBoxBudget<-function(session,DropBox_dir,local_dir,budget=input$budget_year,tab=input$tabset,use_DropBox=T){

      #Get the tree data
      Treedata<-getTreeMapData(DropBox_dir,local_dir,budget=budget,data_treemap="/data_treemap.csv",use_DropBox=use_DropBox)

      #Get the tab data
      Tabdata<-getTabData(DropBox_dir,local_dir,tab=tab,budget=budget,use_DropBox=use_DropBox)

      data<-list()
      data$TabData<-Tabdata
      data$TreeData<-Treedata

      #Send both tree data and tabset data to the client
      data<-jsonlite::toJSON(data)
      session$sendCustomMessage(type="updatebudget",data)

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

