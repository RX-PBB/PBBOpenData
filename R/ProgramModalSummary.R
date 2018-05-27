#**************************************************
#
#  Functions for OpenData Program Summary Modal
#
#**************************************************





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
