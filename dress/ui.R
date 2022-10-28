library(shiny)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinyfullscreen)
library(bs4Dash)
library(shinycssloaders)
library(shinyFiles)
library(shinyjs)
library(waiter)
library(htmltools)
library(shinyBS)

library(DT)
library(gt)
library(tidyverse)
library(plotly)
library(here)
library(scales)
library(gghighlight)
library(ggthemes)
library(crosstalk)
library(flexdashboard)

# 
# source(here::here("analysis/DisclosureRisk.R"))
# source(here::here("analysis/DRisk_update.R"))

ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ...")),
  title = "Disclosure Risk Assessment",
  fullscreen = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "dress",
      color = "primary",
      href = "https://github.com/mohammedfaizan0014/dress",
      image = icon("github"),
      opacity=1
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = FALSE
  ), #header
  ## Sidebar content
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    sidebarUserPanel(
      image = "dashboard.svg",
      name = "Measure Disclosure Risk"
    ),
    sidebarMenu(id="sidebar",
                sidebarHeader("Data and Parameters"),
                menuItem("About", tabName = "about", icon = ionicon(name="information-circle")),
                menuItem("Help Center", tabName = "help", icon = ionicon(name="information-circle")),
                menuItem("Contact Us", tabName = "contact", icon = ionicon(name="call")),
                menuItem("FAQ", tabName = "faq", icon = ionicon(name="help-circle")),
                menuItem("drscore", tabName = "drscore", icon = ionicon(name="arrow-forward"),selected=TRUE),
                menuItem("Sample Data", tabName = "sample", icon = ionicon(name="arrow-forward")),
                menuItem("Protected Data", tabName = "protected", icon = ionicon(name="arrow-forward"))

    )
  ), #sidebar
  footer = dashboardFooter(
    left = a(
      href = "bradleyw@uow.edu.au",
      target = "_blank", "Copyright © Bradley Wakefield",a(
        href = "https://www.uow.edu.au/",
        target = "_blank", ", University of Wollongong"
      )
    ),
    right =  actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                          onclick = sprintf("window.open('%s')",
                                            "https://twitter.com/intent/tweet?text=%20@UOW%20Share%20drscore&url=https://github.com/mohammedfaizan0014/dress&hashtags=DisclosureRisk"))
  ), #footer
  body  = dashboardBody(
    #tags$head(includeScript(here::here("js/baidu_analysis.js"))),
    tabItems(
      tabItem(tabName = "drscore",
              fluidRow(
                box(title="Disclore Risk Information",solidHeader=TRUE,status='primary',background = "white", width=8,
                    fluidRow(
                      
                      uiOutput("intro"),
                      
                      column(6,
                             uiOutput("rsam"),
                             gaugeOutput("risksample", width = "100%", height = "200px")
                             ),
                      column(6,
                             uiOutput("osam"),
                             gaugeOutput("riskoutlier", width = "100%", height = "200px"))
                    ),
                    #fluidPage(uiOutput("intro")),
                    gt_output("linkcounts"),
                    uiOutput("hline"),
                    fluidRow(
                      valueBoxOutput("distinct"),
                      valueBoxOutput("estimated"),
                      valueBoxOutput("undeniable")

                    ),
                    box(title="Category Level Disclosure Risk:",solidHeader=TRUE,status='primary',background = "white", width=12,

                        dataTableOutput("LinkScore_Levels",width = "95%", height = "60%")
                    ) #box
                    ), #box
                #tags$hr(),
                box(width=4,
                    # Input: Select a file ----
                    actionBttn(
                      inputId = "rune",
                      label = "run example",
                      style = "fill",
                      color = "warning",
                      size = "sm",
                    ),
                    tags$hr(),
                    #Sample
                    tags$h5("Upload Sample Data (csv files only)"),
                    actionBttn(
                      inputId = "show",
                      label = "view example file",
                      style = "fill",
                      color = "primary",
                      size = "sm",
                    ),
                    tags$br(),
                    tags$br(),
                    fileInput("sample",NULL,
                              multiple = FALSE,
                              accept = ".csv"),
                    #Protected
                    tags$h5("Upload Protected/Released Data (csv files only)"),
                    actionBttn(
                      inputId = "show1",
                      label = "view example file",
                      style = "fill",
                      color = "primary",
                      size = "sm",
                    ),
                    tags$br(),
                    tags$br(),
                    fileInput("protected",NULL,
                              multiple = FALSE,
                              accept = ".csv"),
                    #delta
                    numericInput("delta", "Delta", value=0.05, min=0, max=5),
                    #neighbourhood
                    selectInput('neighbourhood', 'neighbourhood',
                                c("Mahalanobis",
                                  "DSTAR",
                                  "StdEuclid",
                                  "RelEuclid"),
                                selected = "Mahalanobis"),
                    #kdistinct
                    numericInput("kdistinct", "kdistinct", value=5, min=0, max=5),
                    #ldeniable
                    numericInput("ldeniable", "ldeniable", value=0.05, min=0, max=5),
                    #neigh_type
                    selectInput('neigh_type', 'neigh_type',
                                c("constant",
                                  "prob",
                                  "estprob"),
                                selected = "constant"),
                    #numeric.vars
                    textInput('nvar1ip', 'Enter numeric variable names (comma delimited)', "NULL"),
                    textInput('nvar2ip', 'Enter position of numeric variable name (comma delimited)', "NULL"),
                    numericRangeInput(
                      inputId = "nvar3ip", label = "Enter range of numeric variable:",
                      value = NULL,
                      min=1,
                      step = 1
                    ),
                    # outlier.par = list(centre = median,
                    #                    scale = var,
                    #                    thresh = 0.01)
                    #centre
                    # selectInput('centre', 'centre',
                    #             c("median",
                    #               "mean"),
                    #             selected = "median"),
                    # #scale
                    # selectInput('scale', 'scale',
                    #             c("var",
                    #               "sd"),
                    #             selected = "var"),
                    #thresh
                    numericInput("thresh","thresh", value=0.01, min=0, max=5),

                    #buttons drscore and update
                    actionButton("drscore", "drscore"),
                    actionButton("update", "update"),
                    dropdownButton(
                      downloadBttn(
                        outputId = "linkcountd",
                        label="Linkcounts",
                        style = "fill",
                        color = "success",
                        #size='sm',
                        block=TRUE
                      ),
                      downloadBttn(
                        outputId = "linkscored",
                        label="Linkscores",
                        style = "fill",
                        color = "success",
                        #size='sm',
                        block=TRUE
                      ),
                      downloadBttn(
                        outputId = "LinkScore_Levelsd",
                        label="LinkScore_Levels",
                        style = "fill",
                        color = "success",
                        #size='sm',
                        block=TRUE
                      ),
                      circle=FALSE,
                      label="Download Results",
                      status="success"
                    ) #dropdown btn
                ) #box
      ) #fluid row

      ), #tabItem
      tabItem(tabName = "about",
              fluidRow(box(width=12,
                           title="About",solidHeader=TRUE,status='primary',background = "white",height="100%",
                           tags$h2("Help Center"),
                           tags$hr(),
                           tags$p(""),
                           tags$img(src="x1.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="format.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="heatmap1.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="show.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="heatmap2.png"),
                           tags$img(src="show1.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="settings.png",width="50%",height="50%")

              )
              )
      ),
      tabItem(tabName = "help",
              fluidRow(box(width=12,
                           title="Help Center",solidHeader=TRUE,status='primary',background = "white",height="100%",
                           tags$h2("Help Center"),
                           tags$hr(),
                           tags$p(""),
                           tags$img(src="x1.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="format.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="heatmap1.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="show.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="heatmap2.png"),
                           tags$img(src="show1.png",width="50%",height="50%"),
                           tags$p(""),
                           tags$img(src="settings.png",width="50%",height="50%")

              )
              )
      ),
      tabItem(tabName = "contact",
              fluidRow(box(width=12,
                           title="Contact Us",solidHeader=TRUE,status='primary',background = "white",height=800,
                           tags$p(""),
                           tags$p("")
              ))
      ),
      tabItem(tabName = "faq",
              fluidRow(box(width=12,
                           title="FAQ",solidHeader=TRUE,status='primary',background = "white",height=800,
                           tags$h2("FAQ"),
                           tags$hr(),
                           tags$p(""),
                           tags$p(""),
                           tags$p(""),
                           tags$p(""),
                           tags$p("")
              )
              )
      ), #tabItem
      tabItem(tabName="sample",
              dataTableOutput("sample",width = "100%", height = "100%")),
      tabItem(tabName="protected",
              dataTableOutput("protected",width = "100%", height = "100%"))
    ) #tabItem
  ) #dashboardbody
) #ui
