#**************************************************
#
#  Functions for OpenData Program Summary Modal
#
#**************************************************


#' Program Summary Modal
#'
#' Creates a modal summary of a program. You may select to only include a header summary or select what tabs to include.
#' @param Modal_header Summary of data echo'd back from the csv setup file
#' @param Modal_tabs Include summary of Total, Positions, Operating Costs
#' @param TotalCost Pie chart of Personnel and NonPersonnel
#' @param Positions Include Position Pie and Table
#' @param OperatingCosts Include operating cost pie and table
#' @param Program Program name from onClick function client side
#' @param Desc Desc from onClick function client side
#' @param description.HTML Use HTML in description
#' @param TotalCost Total Cost from onClick function client side
#' @param FTE FTE from onClick function client side
#' @export
#' @examples
#' showModal(ProgramModal(Modal_header=T,Modal_tabs=F,TotalCost=T,Positions=T,OperatingCosts=T))
#' showModal(ProgramModal(Modal_header=T,Modal_tabs=T,TotalCost=T,Positions=T,OperatingCosts=T))


ProgramModal<-function(Modal_header=T,Modal_tabs=T,TotalCost_tab=T,Positions_tab=NULL,OperatingCosts_tab=NULL,Suggestions_tab=NULL,
                       Program=input$chartdata_Program,
                       Desc=input$chartdata_Desc,description.HTML=F,
                       TotalCost=input$chartdata_TotalCost,
                       FTE=input$chartdata_FTE,
                       hasProgramMetrics=F,
                       MetricsLink=NULL){

  if(is.null(Positions_tab))(Positions_tab<-T)
  if(is.null(OperatingCosts_tab))(OperatingCosts_tab<-T)
  if(is.null(Suggestions_tab))(Suggestions_tab<-F)

  if(Modal_header==T){

    #if we include a tabset, put this break in.
    if(Modal_tabs==T)(rule<-hr())else(rule<-NULL)

    if(description.HTML==F){
      description<-p(strong('Description: '),Desc)
    }
    if(description.HTML==T){
      description<-HTML(paste(strong('Description: '),Desc,sep=''))
    }

    Modal_header<- tagList(
          description,
          p(strong('Total Cost: '),format(as.numeric(TotalCost),big.mark=',')),
          p(strong('FTE: '),round(as.numeric(FTE),digits=2)),
          rule
    )

  }

  if(TotalCost_tab==T)(TotalCost_tab<-tabPanel('Total Cost',htmlOutput("ProgramTotal_GvizPlot")))else(TotalCost_tab<-NULL)

  if(Positions_tab==T){Positions_tab<-tabPanel('Positions',
                     #uiOutput('PersonnelCosts_UI'),
                      htmlOutput("ProgramPersonnel_GvizPlot"),
                      rHandsontableOutput('ProgramPersonnel_hot')
            )}else(Positions_tab<-tabPanel(""))

  if(OperatingCosts_tab==T){OperatingCosts_tab<-tabPanel('Operating Costs',
                     #uiOutput('NonPersonnelCosts_UI'),
                      htmlOutput("ProgramNonPersonnel_GvizPlot"),
                      rHandsontableOutput('ProgramNonPersonnel_hot')
            )}else(OperatingCosts_tab<-tabPanel(""))


  if(Suggestions_tab==T){Suggestions_tab<-tabPanel('Suggestion Box',icon=icon('envelope'),br(),
                       fluidRow(column(10,offset=1,uiOutput('SuggestionBox_UI'),br(),
                          tags$div(class="form-group shiny-input-container",
                            HTML('<textarea id="suggestion_comment" rows="4" cols="65"></textarea>
                                  <button id="suggestion_submit" type="button" class="btn btn-info action-button">Submit</button>')
                          )
                       ))


  )}else(Suggestions_tab<-tabPanel(""))


  if(hasProgramMetrics==T){

    Metrics_tab<-tabPanel('Performance',

                    HTML(paste0('<iframe src="',MetricsLink,'" style="width:100%;height:350px;"></iframe>'))

    )

  }else{Metrics_tab<-tabPanel("")}

  if(Modal_tabs==T){

    Modal_tabs<-tagList(tabsetPanel(id='ProgramModal_tabs',TotalCost_tab,Positions_tab,OperatingCosts_tab,Metrics_tab,Suggestions_tab))

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

#' subset_SummaryAll
#'
#' subsets data
#' @param df Data to subset
#' @param select_by column to select on.
#' @param filter row element to choose. Example, Prg# = 510
#' @param Cost.Type Option to subset by Personnel or NonPersonnel
#' @param AcctType Revenue or Expense, for our pie charts assume it is all expense
#' @export
#' @examples
#' subset_SummaryAll(df=SummaryAll,select_by='Prg#',filter=chartdata_Program(),Cost.Type=NULL)

subset_SummaryAll<-function(df,select_by,filter,Cost.Type=NULL,AcctType='Expense'){

  df<-df[which(is.element(df[,select_by],filter)),]

  if(!is.null(Cost.Type))(df<-df[df[,'Cost.Type']==Cost.Type,])

  #if set to NULL return both Revenue and Expense
  if(!is.null(AcctType))(df<-df[df$AcctType==AcctType,])

  return(df)


}


#-------------------------------------------------
#
#  gVis Pie's
#
#-------------------------------------------------

#' rollup_gvisPie
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

  temp<-temp[temp[,numvar]>0,]
  return(temp)

  }




#' gVis_Pie_ProgramCostSummary
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
       if(nrow(df)==0)(return(NULL))

       title_sum<-sum(df[,numvar],na.rm = T)

       df<-rollup_gvisPie(df,labelvar = labelvar, numvar = numvar)

       df[,numvar]<-round(df[,numvar],digits=0)
       df<-df[order(-df[,numvar]),]
       colnames(df)[1]<-labelvar

       dataTitle<-paste(dataTitle,format(round(title_sum,digits = 0),big.mark = ','),sep='')

       gvisPieChart( df, labelvar = labelvar, numvar = numvar,
                  options=list(
                    height=350,
                    title=dataTitle,
                    legend= "{position: 'labeled', alignment: 'start' }",
                    pieSliceText='value',
                    sliceVisibilityThreshold= sliceVisibilityThreshold,
                    pieHole=0.0))

}


#-------------------------------------------------
#
#  rhandson Tables
#
#-------------------------------------------------

#' rhandson_ProgramCostSummary
#'
#' Makes a cost summary tabel for our Program Summary Modal
#' @param df Cost data to summarize
#' @param height Table height
#' @export
#' @examples
#' showModal(ProgramModal(Modal_header=T,Modal_tabs=F,TotalCost=T,Positions=T,OperatingCosts=T))


rhandson_ProgramCostSummary<-function(df,height=175){

    df<-df[order(-df$Cost),]

    hot<-rhandsontable(df, rowHeaders = F,stretchH='all',readOnly=T,height=height,fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>%
                hot_context_menu(allowRowEdit = FALSE, manualColumnMove=T,allowColEdit = FALSE) %>%
                hot_cols(columnSorting = F) %>% #,colWidths=col.widths) %>%
                hot_col('Cost', format="0,0") %>%
                hot_col('Allocation', format="0.00")
    return(hot)
}
