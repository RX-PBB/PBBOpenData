#' helper function for making data for gvis plots
#'
#' Sends email using mailR
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
#' Sends email using mailR
#' @param df Program summary data
#' @param dataTitle Title for cost summary. Example "Total Cost: " or "Personnel: "
#' @param labelvar label variable, example "Personnel"
#' @param numvar Number to sum, example "Cost"
#' @export
#' @examples
#' rollup_gvisPie(df,labelvar='Cost Type',numvar='Cost')

gVis_Pie_ProgramCostSummary<-function(df,dataTitle='Total Cost: ',labelvar='Cost Type',numvar='Cost'){

       df<-rollup_gvisPie(df,labelvar = 'Cost Type',numvar = numvar)

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
                    pieHole=0.0))

}
