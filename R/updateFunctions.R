#**************************************************
#
#  Functions for Updating Data
#
#**************************************************


#' update_Tabs
#'
#' updates the tabset
#' @param tabset input selector
#' @param bpas path to bpas tabs json file
#' @param results path to results tabs json file
#' @export
#' @examples
#' update_Tabs(tabset=input$tabset,bpas='./www/config/roswell_bpas.json',results='./www/config/roswell_tabs.json')


update_Tabs<-function(tabset,bpas,results,session){

  if (is.null(tabset)){
     data<-fromJSON(file=results)
     session$sendCustomMessage(type="updatetabs",toJSON(data))
  }


  if (tabset=='Program Attributes'){

     data<-fromJSON(file=bpas)
     session$sendCustomMessage(type="updatetabs",toJSON(data))

     #session$sendCustomMessage(type="updatetabs_height",160)

  }

  if (tabset=='Results'){

     data<-fromJSON(file=results)
     session$sendCustomMessage(type="updatetabs",toJSON(data))

     #session$sendCustomMessage(type="updatetabs_height",235)

  }

}
