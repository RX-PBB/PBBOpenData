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
#' page_head(title="PBB",favicon="favicon.ico")

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
#' app_header(header='header.jpg',info.top=245,info.left=20,header_logo=NULL,header_logo.top=NULL,header_logo.left=NULL,tabs.height=225)

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

#' app_header
#'
#' Sets the UI of charts for our app across a set of tabs
#' @export
#' @examples
#' app_charts()


app_charts<-function(){

  tagList(
      fluidRow(column(4,selectInput('tabset','Select a Tabset',choices='loading',width='100%')),
               column(4,selectInput('budget_year','Select a Budget',choices='loading',width='100%')),
               column(4,selectInput('department','Select a Department',choices='All Departments',width='100%'))),


      tabsetPanel(id="chart_tabs",

              tabPanel('TreePlot',
                       h4(em("Click into the boxes below for more detail, you can click down to view details on Departments, Divisions, and Programs. Click \"Overall\" to return to the top.")),

                       tags$div(id="treemap", style="width:1100px; height:700px; overflow:hidden")
              ),
              tabPanel("Table",
                       br(),

                       fluidRow(column(12,DT::dataTableOutput('ProgramResultsTable'))),
                        tags$script("$(document).on('click', '#ProgramResultsTable button', function () {
                        Shiny.onInputChange('lastClickId',this.id);
                        Shiny.onInputChange('lastClick', Math.random())});")
              )
      )
  )


}

#' app_footer
#'
#' Does the layout of the tabs and loads organization header image. Also set some locations of our info modal and optional org logo
#' @param logo org footer logo
#' @param logo.top_margin some spacing for the org logo to top of the footer
#' @param height footer height
#' @param background_color color of the bottom footer
#' @param rx_logo resourcex logo saved in assets
#' @param rx_logo.top_margin some spacing between org logo and the rx logo at the bottom
#' @param copyright copyright text
#' @export
#' @examples
#' app_header(header='header.jpg',info.top=245,info.left=20,header_logo=NULL,header_logo.top=NULL,header_logo.left=NULL,tabs.height=225)

app_footer<-function(logo,logo.top_margin=20,height=205,background_color="#eee",rx_logo='resourcex_logo.png',rx_logo.top_margin=35,copyright="2018 Copyright City of"){

  tagList(
    tags$div(id='rx-footer',style=paste0("width:1100px; height:",height,"px; background-color:",background_color,";margin-top:10px;")),

    HTML(paste0("<center><img src='./assets/",logo,"' style='margin-top:",logo.top_margin,"px;'></center>")),

    tags$div(style="width:1100px; display:inline-block;",

        HTML(paste0("<div class='col-sm-6' align='center' style='margin-top:",rx_logo.top_margin,"px;'><em><p class='rx-footer'>&copy; ",copyright,"</p></em> </div>")),
        HTML(paste0("<div class='col-sm-6' align='center' style='margin-top:",rx_logo.top_margin,"px;'><em><p style='display:inline;'>Powered by:&nbsp;&nbsp;</p></em><a href='https://www.resourcex.net/blog' target='_blank'><img src='./assets/",rx_logo,"'> </a></div>"))

    )


  )


}


