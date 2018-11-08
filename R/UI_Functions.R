#**************************************************
#
#  Functions for making the open data UI
#
#**************************************************


#' page_head
#'
#' loads libraries and sets page title and favicon
#' @param title pAGE title
#' @param db_host_new database host
#' @export
#' @examples
#' head(title="PBB",favicon="favicon.ico")

page_head<-function(title="PBB",favicon="favicon.ico"){
  tags$head(
    HTML(paste0('
        <meta charset="UTF-8">
        <title>',title,'</title>
        <link rel="icon" href="assets/',favicon,'" type="image/x-icon">
        <link rel="shortcut icon" href="assets/',favicon,'" type="image/x-icon">

        <!-- We use google fonts for many of the examples, but they are not necessary -->
        <link href="https://fonts.googleapis.com/css?family=Roboto:600,400,200" rel="stylesheet" type="text/css">
        <link href="https://fonts.googleapis.com/css?family=Raleway:600, 400" rel="stylesheet" type="text/css">


     <!-- Bootstrap -->

        <link rel="stylesheet" href="lib/styles/bootstrap.css">
        <script src="lib/bootstrap.min.js"></script>

      <!-- Shiny

        Use this if HTML template only, watch out for double source of jquery

        <script src="shared/shiny.js" type="text/javascript"></script>
        <link rel="stylesheet" type="text/css" href="shared/shiny.css"/>
        <script src="shared/jquery.js" type="text/javascript"></script>

     -->

      <!-- Vizuly -->

        <link rel="stylesheet" href="lib/styles/vizuly.css">
        <script src="lib/d3.min.js"></script>
        <script src="lib/vizuly2_core.min.js"></script>
        <script src="scripts/TabControl.js"></script>
        <script src="scripts/TreeMap.js"></script>
       '))
  )
}


#' app_header
#'
#' Does the layout of the tabs and loads organization header image. Also set some locations of our info modal and optional org logo
#' @param header page header image
#' @param info.top position of info button from the top
#' @param info.left position of info button from the left
#' @param header_logo optional org logo for header
#' @param header_logo.top optional org logo position from top
#' @param header_logo.left optional org logo position from left
#' @param tabs.height height of tab widget
#' @export
#' @examples
#' head(title="PBB",favicon="favicon.ico")

app_header<-function(header='header.jpg',info.top=245,info.left=20,header_logo=NULL,header_logo.top=NULL,header_logo.left=NULL,tabs.height=225){

  tagList(

   #<!-- City Header, info and logos  -->
    tags$div(id="header-image", style="position:relative",
             tags$img(src=paste0("assets/",header)),

             tags$div(style=paste0("position:absolute; font-size:40px; top:",info.top,"px; left:",info.left,"px; color:#FFF; font-weight:bold;"),
               tags$img(src="assets/info.png",style="cursor: pointer;",id="info")
             ),

             if(!is.null(header_logo)){
               tags$div(style=paste0("position:absolute; font-size:40px; top:",header_logo.top,"px; left:",header_logo.left,"px;"),
                  tags$img(src=paste0("assets/",header_logo))
               )
             }
    ),
    #<!-- Tab Widget  -->
    tags$div(style="height:10px"),
    tags$div(id="tabs",style=paste0("width:1100px; height:",tabs.height,"px"))

  )

}

