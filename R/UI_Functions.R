#**************************************************
#
#  Functions for making the open data UI
#
#**************************************************


#' app_head
#'
#' loads libraries and sets page title and favicon
#' @param title pAGE title
#' @param db_host_new database host
#' @export
#' @examples
#' page_head(title="PBB",favicon="favicon.ico")

app_head<-function(title="PBB",favicon="favicon.ico",base='openpbbdata.net/present'){
  tagList(
    HTML(paste0('
                <meta charset="UTF-8">
                <title>',title,'</title>
                <link rel="icon" href="assets/',favicon,'" type="image/x-icon">
                <link rel="shortcut icon" href="assets/',favicon,'" type="image/x-icon">

                <!-- We use google fonts for many of the examples, but they are not necessary -->
                <link href="https://fonts.googleapis.com/css?family=Roboto:600,400,200" rel="stylesheet" type="text/css">
                <link href="https://fonts.googleapis.com/css?family=Raleway:600, 400" rel="stylesheet" type="text/css">


                <!-- Bootstrap -->


                <link rel="stylesheet" href="https://',base,'/assets/lib/styles/bootstrap.css">
                <script src="https://',base,'/assets/lib/bootstrap.min.js"></script>


                <!-- Vizuly -->

                <link rel="stylesheet" href="https://',base,'/assets/lib/styles/vizuly.css">
                <script src="https://',base,'/assets/lib/d3.min.js"></script>
                <script src="https://',base,'/assets/lib/vizuly2_core2.min.js"></script>
                <script>

                vizuly3 = vizuly2;

                </script>
                <script src="https://',base,'/assets/lib/vizuly2_core.min.js"></script>
                <script src="https://',base,'/assets/scripts/TabControl.js"></script>
                <script src="https://',base,'/assets/scripts/TreeMap.js"></script>
                <script src="https://',base,'/assets/scripts/BarChart_viz2.js"></script>

                <!-- Specialized -->
                <link rel="stylesheet" href="https://',base,'/assets/lib/styles/spinny.css">

                <!-- introjs -->
                <link rel="stylesheet" href="https://',base,'/assets/lib/styles/introjs.css">
                <script src="https://',base,'/assets/scripts/intro.js"></script>
                '))

    )
}



#' app_spinner
#'
#' put this in the head funtion to use a busy spinner. USees gif file saved in assets
#' @param spinner spinner.gif file saved in assets
#' @export
#' @examples
#' app_spinner(spinner='spin4.gif')
#'
#'
app_spinner<-function(spinner='spin4.gif'){
   tagList(
          useShinyjs(),

         shinyjs::extendShinyjs(script="../assets/scripts/shinyjs-funcs.js",functions=c('init')),
        #
        tags$style('
          #rx-busy-overlay {
            height: 100%;
            width: 100%;
            opacity: 0.7;
            position: fixed;
            left: 0;
            top: 0;
            background: #ecf0f5;
            display: none;
            z-index: 2000;
            text-align: center;
            padding-top: 50px;
            font-size: 50px;
            color: #555555;
          }

          #rx-busy-overlay.px-rbb-busy-long {
            display: block;
          }
        '),

        div(id = "rx-busy-overlay",
              #"Loading. Please wait...",
              img(src = paste0("assets/",spinner), height = "100px")

        )
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
#' @param info.click_me additional text can be HTML formatted with style next to info icon
#' @param info.intro run the intro tutorial on page load. False or Null requires user to click info icon in header.
#' @export
#' @examples
#' app_header(header='header.jpg',info.top=245,info.left=20,header_logo=NULL,header_logo.top=NULL,header_logo.left=NULL,tabs.height=225)

app_header<-function(header='header.jpg',info.top=245,info.left=20,header_logo=NULL,header_logo.top=NULL,header_logo.left=NULL,tabs.height=225,info.click_me=NULL,info.intro=NULL){

  if(is.null(info.intro)){
    intro<-paste0("onclick='RunIntro_OnInfo(\"startIntro_PBB\")'")
  }else{
    intro<-paste0("onclick='RunIntro_OnInfo(\"",info.intro,"\")'")
  }

  tagList(
    tags$div(id="spinnyloader", class="spinny"),
    #<!-- City Header, info and logos  -->
    tags$div(id="header-image", style="position:relative",
             tags$img(src=paste0("assets/",header)),

             tags$div(style=paste0("position:absolute; font-size:40px; top:",info.top,"px; left:",info.left,"px; color:#FFF; font-weight:bold;"),
                      #tags$img(src="assets/info.png",style="cursor: pointer;",id="info_bk")
                      HTML(paste0('<a style="cursor: pointer;color:white;" ',intro,'><img src="assets/info.png" id="info_bk"/>',info.click_me,'</a>'))

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

#' app_charts
#'
#' Sets the UI of charts for our app across a set of tabs
#' @param hasMetrics Include the metrics tab
#' @param results_tab Include the summary results tab
#' @param results_chart_height height of summary chart
#' @export
#' @examples
#' app_charts()
app_charts<-function(results_tab=NULL,results_chart_height=500,
                     #include_legend=treemap$include_legend,
                     #legend_position=treemap$legend_position,
                     hasMetrics=NULL){


  # treemap_legend<-NULL
  #
  #
  # if(!is.null(include_legend)){
  #
  #   if(include_legend==TRUE){
  #     treemap_legend<-uiOutput('treemap_legend')
  #   }else{
  #     treemap_legend<-NULL
  #   }
  #
  # }else{
  #   #default other clients to always include unless we come back and specify false
  #   #but for now test as default no on if not specified
  #   #treemap_legend<-uiOutput('treemap_legend')
  #
  #   treemap_legend<-NULL
  # }
  #
  # legend_top<-NULL
  # legend_bottom<-NULL
  #
  # if(!is.null(legend_position)){
  #
  #   if(legend_position=='top'){
  #
  #     legend_top<-HTML(paste0('<h4>
  #         <em>In the plot below, each box represents a program. The size of the box shows cost.
  #             Box shade indicates how significant the program is to achieving the result.
  #             The darker the shade the more aligned the program is with the selected result.</em>
  #       </h4>',treemap_legend))
  #
  #
  #   }else{
  #
  #    legend_bottom<-HTML(paste0(treemap_legend,'<h4>
  #         <em>In the plot above, each box represents a program. The size of the box shows cost.
  #             Box shade indicates how significant the program is to achieving the result.
  #             The darker the shade the more aligned the program is with the selected result.</em>
  #       </h4>'))
  #
  #   }
  #
  #
  # }


  chart_tabs<-tagList(
    fluidRow(column(4,selectInput('tabset','Select a Tabset',choices='loading',width='100%')),
             column(4,selectInput('budget_year','Select a Budget',choices='loading',width='100%')),
             column(4,selectInput('department','Select a Department',choices='All Departments',width='100%'))),


    shiny::tabsetPanel(id="chart_tabs",

                shiny::tabPanel('TreePlot',

                             h4(em("Click into the boxes below for more detail, you can click down to view details on Departments, Divisions, and Programs. Click \"Overall\" to return to the top.")),

                             #legend_top,

                              tags$div(id="treemap", style="width:1100px; height:700px; overflow:hidden")

                             # legend_bottom


                ),
                shiny::tabPanel("Table",
                         h4(em("The table shows the highest cost program by category. Click more info to see a detailed breakdown of the program costs.")),
                         br(),
                         fluidRow(column(12,DT::dataTableOutput('ProgramResultsTable'))),
                         tags$script("$(document).on('click', '#ProgramResultsTable button', function () {
                                     Shiny.onInputChange('lastClickId',this.id);
                                     Shiny.onInputChange('lastClick', Math.random())});"),

                         h4(em("**Please allow 5-10 seconds for the table to load"))
                )#,

                #metrics_tab




    ),

    hr()
  )

  if(!is.null(results_tab)){

     if(results_tab==T){

      chart_tabs<-tagList(
        fluidRow(column(4,selectInput('tabset','Select a Tabset',choices='loading',width='100%')),
                 column(4,selectInput('budget_year','Select a Budget',choices='loading',width='100%')),
                 column(4,selectInput('department','Select a Department',choices='All Departments',width='100%'))),

        shiny::tabsetPanel(id="chart_tabs",

                    shiny::tabPanel('Summary',

                             tags$div(id="stackedbar", style=paste0("width:1100px; height:",results_chart_height,"px; overflow:hidden"))
                    ),
                    shiny::tabPanel('TreePlot',

                              h4(em("Click into the boxes below for more detail, you can click down to view details on Departments, Divisions, and Programs. Click \"Overall\" to return to the top.")),

                              #legend_top,

                              tags$div(id="treemap", style="width:1100px; height:700px; overflow:hidden")

                              #legend_bottom


                    ),
                    shiny::tabPanel("Table",
                             h4(em("The table shows the highest cost program by category. Click more info to see a detailed breakdown of the program costs.")),
                             br(),
                             fluidRow(column(12,DT::dataTableOutput('ProgramResultsTable'))),
                             tags$script("$(document).on('click', '#ProgramResultsTable button', function () {
                                         Shiny.onInputChange('lastClickId',this.id);
                                         Shiny.onInputChange('lastClick', Math.random())});"),

                             h4(em("**Please allow 5-10 seconds for the table to load"))
                    )#,

                    #metrics_tab




        ),

        hr()
      )


     }}


  return(chart_tabs)
}


#' app_aboutpbb
#'
#' Does the layout of the about section
#' @param about.link A link to their page, typically finance or city manager or maybe they have a PBB page
#' @param about.who About who...
#' @param about.text html of some paragraphs on why PBB
#' @export
#' @examples
#' app_aboutpbb(about.link,about.who,about.text)


app_aboutpbb<-function(about.link,about.who,about.text,about.what=' Priority Based Budgeting',info.intro=NULL){

  if(is.null(info.intro)){
    intro<-paste0("onclick='RunIntro_OnInfo(\"startIntro_PBB\")'")
  }else{
    intro<-paste0("onclick='RunIntro_OnInfo(\"",info.intro,"\")'")
  }

  tagList(
    column(6,
           h3(style='margin-top:5px;',tags$a(href=about.link,target="_blank",strong("About ",about.who,about.what))),
           h4(style='margin-top:5px;',
              HTML(about.text)
           ),
          HTML(paste0("<h4  style='font-weight: 800;cursor: pointer;'",intro," id='info_bk'>
        <em>The visualization above reflects the cost of our services, and how they align with our strategic goals for the community. <font style='color: #1e3ff4;'>Click for more info.</font></em>
        </h4>"))
    )
  )

}

#' app_feedback
#'
#' Does the feedback layout
#' @export
#' @examples
#' app_feedback()

app_feedback<-function(){

  tagList(
    column(6,
       HTML('<div id="app_feedback">
        <h3 style="margin-top:5px;"><strong>Feedback</strong></h3>

        <h4 style="margin-top:5px;">We want to hear from you </h4>

        <div class="form-group shiny-input-container" style="width: 500px;">
            <input id="name" type="text" class="form-control" value="Name"/>
        </div>

        <div class="form-group shiny-input-container" style="width: 500px;">
            <input id="email" type="text" class="form-control" value="Email"/>
        </div>

        <div class="form-group shiny-input-container">

              <textarea id="comment" rows="4" cols="65">&nbsp;&nbsp; Comments and Suggestions</textarea>
        </div>

          <button id="submit_feedback" type="button" class="btn btn-info action-button">Submit</button>

        </div></div>'

    )))

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
#' app_footer(logo,logo.top_margin=20,height=205,background_color="#eee",rx_logo='resourcex_logo.png',rx_logo.top_margin=35,copyright="2018 Copyright City of")

app_footer<-function(logo,logo.top_margin=20,height=205,background_color="#eee",rx_logo='resourcex_logo.png',rx_logo.top_margin=35,copyright="2018 Copyright City of"){

  tagList(
    tags$div(style='margin-top:10px;'),
    tags$div(id='rx-footer',style=paste0("width:1100px; height:",height,"px; background-color:",background_color,";margin:0px auto;"),

      HTML(paste0("<center><img src='./assets/",logo,"' style='margin-top:",logo.top_margin,"px;'></center>")),

      tags$div(style="width:1100px; display:inline-block;",

          HTML(paste0("<div class='col-sm-6' align='center' style='margin-top:",rx_logo.top_margin,"px;'><em><p class='rx-footer'>&copy; ",copyright,"</p></em> </div>")),
          HTML(paste0("<div class='col-sm-6' align='center' style='margin-top:",rx_logo.top_margin,"px;'><em><p style='display:inline;'>Powered by:&nbsp;&nbsp;</p></em><a href='https://www.resourcex.net/blog' target='_blank'><img src='./assets/",rx_logo,"'> </a></div>"))

      )
    )
  )

}

#' app_endjs
#'
#' loads javascript files at end up ui that are same for all
#' @export
#' @examples
#' app_endjs()
app_endjs<-function(){
  tagList(
    HTML('<script src="https://openpbbdata.net/present/assets/scripts/tools.js"></script>'),
    HTML('<script src="https://openpbbdata.net/present/assets/scripts/openpbbdata_intro.js"></script>')
  )
}
