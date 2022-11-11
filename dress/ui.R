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
                           tags$h1("Dress"),
                           tags$hr(),
                           tags$p("The dress package provides some measures for disclosure risk associated with the release of protected data, irrespective of what mechanism was used to protect it. Key principles of the disclosure framework include distinctness, accuracy and undeniability. This method can be applied to any pair of original and protected data-sets despite a difference in dimensionality and without assuming any particular joint probability structure between the original and protected data. "),
                           tags$p("Disclosure is defined in terms of three key principles: distinctness, accuracy and undeniability. More simply put in order to have a disclosure we need the observation value to be a sensitive characteristic (something likely pertaining to the individual only) within our original data-set (distinct), we need to be able to properly estimate this observation based on the release of information (accurately estimated) and we need our estimate to be able to be attributed with this observation with some level of certainty (undeniable)."),
                           tags$h6("Access package repository", tags$a(href="https://github.com/mohammedfaizan0014/dress","here"))
              )
              )
      ),
      tabItem(tabName = "help",
              fluidRow(box(width=12,
                           title="Help Center",solidHeader=TRUE,status='primary',background = "white",height="100%",
                           tags$h2("Help Center"),
                           tags$hr(),
                           tags$p("1. Upload original data"),
                           tags$hr(),
                           tags$p("2. Upload released data"),
                           tags$hr(),
                           tags$p("3. Input parameters and select appropriate distance measures"),
                           tags$hr(),
                           tags$p("4. Enter numeric variables"),
                           tags$hr(),
                           tags$p("5. Get the drscore"),
                           tags$hr(),
                           tags$p("Note: update works after drscore"),
                           tags$p("More information about mathematics behind measuring the disclosure risk can be found in the paper On the Disclosure Risk Framework for Micro-Level Data and on",
                           tags$a(href="https://mohammedfaizan0014.github.io/dress/index.html","the package site."))

              )
              )
      ),
      tabItem(tabName = "contact",
              fluidRow(box(width=12,
                           title="Contact Us",solidHeader=TRUE,status='primary',background = "white",height=800,
                           tags$h3("Report package issues on GitHub"),
                           tags$a(href="https://github.com/mohammedfaizan0014/dress/issues","https://github.com/mohammedfaizan0014/dress/issues")
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
