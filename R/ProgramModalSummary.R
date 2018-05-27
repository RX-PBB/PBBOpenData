#**************************************************
#
#  Functions for OpenData Program Summary Modal
#
#**************************************************


#' Program Summary Modal
#'
#' Creates a modal summary of a program. You may select to only include a header summary or select what tabs to include.
#' @param Modal_header Summary of data echo'd back from the csv setup file
#' @param TotalCost Pie chart of Personnel and NonPersonnel
#' @param Positions Include Position Pie and Table
#' @param OperatingCosts Include operating cost pie and table
#' @export
#' @examples
#' showModal(ProgramModal(Modal_header=T,Modal_tabs=F,TotalCost=T,Positions=T,OperatingCosts=T))
#' showModal(ProgramModal(Modal_header=T,Modal_tabs=T,TotalCost=T,Positions=T,OperatingCosts=T))


ProgramModal<-function(Modal_header=T,Modal_tabs=T,TotalCost=T,Positions=T,OperatingCosts=T,
                       Program=input$chartdata_Program,
                       Desc=input$chartdata_Desc,
                       TotalCost=input$chartdata_TotalCost,
                       FTE=input$chartdata_FTE){

  if(Modal_header==T){

    #if we include a tabset, put this break in.
    if(Modal_tabs==T)(rule<-hr())else(rule<-NULL)

    Modal_header<- tagList(
          p(strong('Description: '),Desc),
          p(strong('Total Cost: '),format(as.numeric(TotalCost),big.mark=',')),
          p(strong('FTE: '),FTE),
          rule
    )

  }

  if(TotalCost==T)(TotalCost<-tabPanel('Total Cost',htmlOutput("ProgramTotal_GvizPlot")))else(TotalCost<-NULL)

  if(Positions==T){Positions<-tabPanel('Positions',
                     #uiOutput('PersonnelCosts_UI'),
                      htmlOutput("ProgramPersonnel_GvizPlot"),
                      rHandsontableOutput('ProgramPersonnel_hot')
            )}else(Positions<-NULL)

  if(OperatingCosts==T){OperatingCosts<-tabPanel('Operating Costs',
                     #uiOutput('NonPersonnelCosts_UI'),
                      htmlOutput("ProgramNonPersonnel_GvizPlot"),
                      rHandsontableOutput('ProgramNonPersonnel_hot')
            )}else(OperatingCosts<-NULL)

  if(Modal_tabs==T){

    Modal_tabs<-tagList(tabsetPanel(TotalCost,Positions,OperatingCosts))

  }else(Modal_tabs<-NULL)



  modalDialog(
        title = Program,size='l',easyClose = T,

        tagList(
          Modal_header,
          Modal_tabs
        )
  )

}



#*********************************************************************
# Subset Function - Sometimes we want ProgID, sometimes we want name
#*********************************************************************

#' helper function for making data for gvis plots
#'
#' subsets data
#' @param df Data to subset
#' @param select_by column to select on.
#' @param filter row element to choose. Example, Prg# = 510
#' @param Cost.Type Option to subset by Personnel or NonPersonnel
#' @export
#' @examples
#' subset_SummaryAll(df=SummaryAll,select_by='Prg#',filter=chartdata_Program(),Cost.Type=NULL)

subset_SummaryAll<-function(df,select_by,filter,Cost.Type=NULL){

  df<-df[which(is.element(df[,select_by],filter)),]

  if(!is.null(Cost.Type))(df<-df[df[,'Cost.Type']==Cost.Type,])

  return(df)


}


#-------------------------------------------------
#
#  gVis Pie's
#
#-------------------------------------------------

#' helper function for making data for gvis plots
#'
#' rolls up data for gvis plots
#' @param df Data to roll up
#' @param labelvar label variable, example "Personnel"
#' @param numvar Number to sum, example "Cost"
#' @export
#' @examples
#' rollup_gvisPie(df,labelvar='Cost Type',numvar='Cost')


rollup_gvisPie<-function(df,labelvar,numvar){

  #***************************************
  # Helper Function
  #***************************************
  temp<-NULL
  df<-split(df,df[,labelvar])
  for(i in 1:length(df)){

    row<-data.frame(labelvar=df[[i]][1,labelvar],
                    numvar=sum(df[[i]][,numvar],na.rm=T))
    temp<-rbind(temp,row)
  }
  colnames(temp)<-c(labelvar,numvar)
  return(temp)

  }




#' Function for making gVis Cost summary of Total Cost, Personnel, or NonPersonnel
#'
#' makes a plot
#' @param df Program summary data
#' @param dataTitle Title for cost summary. Example "Total Cost: " or "Personnel: "
#' @param labelvar label variable, example "Personnel"
#' @param numvar Number to sum, example "Cost"
#' @export
#' @examples
#' rollup_gvisPie(df,labelvar='Cost Type',numvar='Cost')

gVis_Pie_ProgramCostSummary<-function(df,dataTitle='Total Cost: ',labelvar='Cost Type',numvar='Cost',sliceVisibilityThreshold= NULL){

       df<-rollup_gvisPie(df,labelvar = labelvar, numvar = numvar)

       df[,numvar]<-round(df[,numvar],digits=0)
       df<-df[order(-df[,numvar]),]
       colnames(df)[1]<-labelvar

       dataTitle<-paste(dataTitle,format(round(sum(df[,numvar],na.rm = T),digits = 0),big.mark = ','),sep='')

       gvisPieChart( df, labelvar = labelvar, numvar = numvar,
                  options=list(
                    height=350,
                    title=dataTitle,
                    legend= "{position: 'labeled', alignment: 'start' }",
                    pieSliceText='value',
                    sliceVisibilityThreshold= sliceVisibilityThreshold,
                    pieHole=0.0))

}
