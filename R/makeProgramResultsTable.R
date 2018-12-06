#' make_ProgramResultsTable
#'
#' Create a DT table of the pbb data for the chart tabs
#' @param df present pbb data
#' @param buttons what buttons to use
#' @param tabdata_dataField what column of data to include from the present sumary csv data
#' @export
#' @examples
#' make_ProgramResultsTable(df,tabdata_dataField=input$tabdata_dataField, buttons=c('copy', 'excel', 'print'))



make_ProgramResultsTable<-function(df,tabdata_dataField,buttons=c('copy', 'excel', 'print'), doneFun=NULL){

  df[["More Info"]]<-
    paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-default action-button" id=ProgID_',df$`#`,'>More Info</button>


           </div>

           ')


  df<-df[c('More Info',tabdata_dataField,'DirectCost', 'Program','Description')]
  df[,tabdata_dataField]<-as.numeric(df[,tabdata_dataField])
  df<-df[order(-df[,tabdata_dataField],-df$DirectCost),]
  df$DirectCost<-format(round(df$DirectCost,digits=0),big.mark = ",")

  height<-600

  width<-1100

  dt<-datatable(df,escape=F,selection = 'none',rownames=F,extensions = list(Scroller=NULL,Buttons=NULL),
                options = list(stateSave=T,
                               dom = 'Bfrtip',
                               buttons = buttons,
                               searchHighlight = TRUE,
                               autoWidth=TRUE,
                               initComplete = JS("function(settings, json) {$('#spinnyloader').removeClass('spinnyloader')}"),
                               columnDefs = list(list(width =paste(width*.1,'px',sep=''), targets = c(0)),
                                                 list(width =paste(width*.08,'px',sep=''), targets = c(1)),
                                                 list(width =paste(width*.08,'px',sep=''), targets = c(2)),
                                                 list(width =paste(width*.25,'px',sep=''), targets = c(3)),
                                                 list(width =paste(width*.44,'px',sep=''), targets = c(4)),
                                                 list(className='dt-center',targets=c(0:2))
                                                 #list(width ='30%', targets = 1),
                                                 #list(className = 'dt-center', targets = c(0,1,2,3,4,6,7,8))
                               ),
                               deferRender=TRUE,
                               scrollX=FALSE,
                               scrollY=height,
                               scrollCollapse=TRUE,
                               pageLength = 25, lengthMenu = c(25,50,150,250)


                ))
  return(dt)
}

