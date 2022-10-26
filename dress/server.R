library(shiny)
library(shinyalert)
library(shinyvalidate)
library(shinyWidgets)
library(shinythemes)

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


source(here::here("analysis/DisclosureRisk.R"))
source(here::here("analysis/DRisk_update.R"))

server <- function(input, output, session) {

  shinyalert("Welcome",
             "Welcome to the ___ Dashboard!",
             type = "info",
             timer = 10000)

  #input files
  sampletemp <- reactive({
    file <- input$sample
    read.csv(file$datapath,TRUE)
  })
  protectedtemp <-  reactive({
    file2 <- input$protected
    read.csv(file2$datapath,TRUE)
  })



  #
  #   output$intro <- renderUI({
  #
  #     tagList(tags$h3("Disclosure Risk Assessment"),
  #             tags$hr(),
  #             tags$h4("Threshold Neighbourhood with parameters: "),
  #             #tags$img(height = 300 , width = 812, src = "https://c.ndtvimg.com/2020-05/tpj5o4f8_cooking-_625x300_02_May_20.jpg"),
  #             tags$br(),
  #             tags$p("delta, kdistinct, ldeniable:"), input$delta, input$kdistinct, input$ldeniable,
  #             tags$hr())
  #   })






  # numeric.vars = 1:4, #Which Variables are continuous?
  #sample <- input$sample #are variables from sample?
  nvar <- reactive({

    if(input$nvar1ip=="NULL"){
      nvar1ip <- NULL
    }
    else{
      nvar1ip <- as.character(unlist(strsplit(input$nvar1ip,","))) #names of cont variables
    }

    if(input$nvar2ip=="NULL"){
      nvar2ip <- NULL
    }
    else{
      nvar2ip <- NULL
      places <- as.numeric(unlist(strsplit(input$nvar2ip,","))) #position of cont variables
      for (i in 1:length(places)) {
        if(!is.na(places[i])){
          nvar2ip <- append(nvar2ip,places[i])
        }
      }

      #nvar2ip <- as.numeric(unlist(strsplit(input$nvar2ip,","))) #position of cont variables
    }

    if(is.null(input$nvar3ip)){
      nvar3ip <- NULL
    }
    else{
      nvar3ip <- c(input$nvar3ip[1]:input$nvar3ip[2]) #range of cont variables
    }

    # picking only nvar strings in sample
    nvar1ipp <- NULL
    if(!is.null(nvar1ip)){
      for (i in 1:length(nvar1ip)) {
        if(nvar1ip[i] %in% colnames(sampletemp())){
          nvar1ipp <- append(nvar1ipp,nvar1ip[i])
        }
      }
    }


    # picking only nvar numbers in sample
    nvar2ipp <- NULL
    if(!is.null(nvar2ip)){
      for (i in 1:length(nvar2ip)) {
        if(nvar2ip[i] <= length(colnames(sampletemp())) & nvar2ip[i] > 0){
          nvar2ipp <- append(nvar2ipp,nvar2ip[i])
        }
      }
    }



    # picking only nvar numbers range in sample
    nvar3ipp <- NULL
    if(!is.null(nvar3ip)){
      for (i in 1:length(nvar3ip)) {
        if(nvar3ip[i] <= length(colnames(sampletemp()))){
          nvar3ipp <- append(nvar3ipp,nvar3ip[i])
        }
      }
    }




    nvar1ipCols <- sampletemp() %>% dplyr::select(nvar1ipp) %>% colnames()
    nvar2ipCols <- sampletemp()%>% dplyr::select(nvar2ipp) %>% colnames()
    nvar3ipCols <- sampletemp() %>% dplyr::select(nvar3ipp) %>% colnames()
    nvar <- unique(append(append(nvar1ipCols,nvar2ipCols),nvar3ipCols))
    if(length(nvar)==0) nvar <- NULL
    return(nvar)
  })





  neighbourhood <- 1
  neighbourhood <- reactive(case_when(
    input$neighbourhood=="Mahalanobis"~1,
    input$neighbourhood=="DSTAR"~2,
    input$neighbourhood=="StdEuclid"~3,
    input$neighbourhood=="RelEuclid"~4,
    TRUE ~ 1))


  output$colname <- renderPrint({
    colnames(sampletemp())
    neighbourhood()
  }
  )

  observeEvent(input$drscore,{



    if(is.null(input$sample) | is.null(input$protected)){

      shinyalert(title = "Please upload the Data")
      observeEvent(input$update, {

        if(is.null(input$sample) | is.null(input$protected)){

          shinyalert(title = "Please upload the Data",
                     text = "We couldn't find the DRisk Object, please try drscore")
        }
      })
    }

    else{

      observeEvent(input$update, {

        if(is.null(input$sample) | is.null(input$protected)){

          shinyalert(title = "Please upload the Data",
                     text = "We couldn't find the DRisk Object, please try drscore")
        }
      })

      #checking if all nvar is numeric
      if(is.null(nvar())){
        shinyalert(title = "Incorrect Numeric Variables!",
                   text = "Data must have atleast one numeric variable")

      }
      else if(!is.null(nvar())){
        numvartemp <- sampletemp() %>% dplyr::select(nvar())
        sizenumvartemp <- length(nvar())

        for(i in 1:sizenumvartemp){
          if(!(is.numeric(numvartemp[,i]))){
            shinyalert(title = "Incorrect Numeric Variables!")
          }
        }
      }
      if(!is.null(nvar())){
        nn <- drscore(Sample = sampletemp(),
                      Protected = protectedtemp(),
                      delta = input$delta,
                      kdistinct = input$kdistinct,
                      ldeniable = input$ldeniable,
                      neighbourhood = neighbourhood(),
                      neigh_type = input$neigh_type,
                      numeric.vars = nvar(),
                      outlier.par = list(centre = median,
                                         scale = var,
                                         thresh = input$thresh))
        linkscore <- nn$Linkscore %>%
          mutate(InvValues=(1-Values)*100)
        output$risksample <- renderGauge({
          gauge(linkscore[,2][1], min = 0, max = 100, symbol = '%', gaugeSectors(
            success = c(80, 100), warning = c(40,79), danger = c(0, 39)
          ))#gauge
        })

        output$riskoutlier <- renderGauge({
          gauge(linkscore[,2][2], min = 0, max = 100, symbol = '%', gaugeSectors(
            success = c(80, 100), warning = c(40,79), danger = c(0, 39)
          ))#gauge
        })

        linkcounts <- nn$Linkcounts
        output$linkcounts <- render_gt({
          linkcounts  %>%
            mutate(`Values`=scales::comma(as.numeric(`Values`))) %>%
            gt(rownames_to_stub=TRUE,rowname_col="") %>%
            tab_header(title = "Linkcounts",
                       subtitle = "Key Diifferences in Observations") %>%
            #tab_source_note(md("drscore")) %>%
            cols_align(
              align = c("right"),
              columns = c(`Values`)
            )
        })

         output$distinct <-  renderValueBox({
           bs4Dash::valueBox(
            subtitle= "Proportion Distinct",
            value=scales::percent(linkscore[,1][3]),
            icon = icon("percent"),
            width = 12
          )
        })

        output$estimated <-  renderValueBox({
          bs4Dash::valueBox(
            subtitle="Proportion Estimated",
            value=scales::percent(linkscore[,1][4]),
            icon = icon("percent"),
            width = 12
          )
        })

        output$undeniable <-  renderValueBox({
          bs4Dash::valueBox(
            value=scales::percent(linkscore[,1][5]),
            subtitle="Proportion Undeniable",
            icon = icon("percent"),
            width = 12
          )
        })

        LinkScore_Levels <- nn$LinkScore_Levels %>%
          mutate_if(is.numeric, round,2)
        output$LinkScore_Levels <- renderDataTable({
          LinkScore_Levels %>% datatable(escape = FALSE, class = 'cell-border stripe',
                                         caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Category Level Disclosure Risk: '),
                                         rownames = TRUE,
                                         options = list(searching = TRUE,pageLength = 10,scrollY=TRUE, scrollX = TRUE))
        })

        output$linkcountd <- downloadHandler(
          filename=function() {
            paste("linkcounts-", Sys.Date(), ".csv", sep="")
          },
          content = function(file){
            write.csv(linkcounts,file)
          }
        )

        output$linkscored <- downloadHandler(
          filename=function() {
            paste("linkscores-", Sys.Date(), ".csv", sep="")
          },
          content = function(file){
            write.csv(linkscore,file)
          }
        )

        output$LinkScore_Levelsd <- downloadHandler(
          filename=function() {
            paste("LinkScore_Levels-", Sys.Date(), ".csv", sep="")
          },
          content = function(file){
            write.csv(LinkScore_Levels,file)
          }
        )



        observeEvent(input$update, {

          if(is.null(input$sample) | is.null(input$protected)){

            shinyalert(title = "Please upload the Data",
                       text = "We couldn't find the DRisk Object, please try drscore")
          }

          else{
            nnupdate <- updateDRisk(DRisk=nn,
                                    delta = input$delta,
                                    kdistinct = input$kdistinct,
                                    ldeniable = input$ldeniable,
                                    neighbourhood = neighbourhood(),
                                    neigh_type = input$neigh_type,
                                    numeric.vars = nvar(),
                                    outlier.par = input$thresh)


            linkscore <- nnupdate$Linkscore %>%
              mutate(InvValues=(1-Values)*100)
            output$risksample <- renderGauge({
              gauge(linkscore[,2][1], min = 0, max = 100, symbol = '%', gaugeSectors(
                success = c(80, 100), warning = c(40,79), danger = c(0, 39)
              ))#gauge
            })

            output$riskoutlier <- renderGauge({
              gauge(linkscore[,2][2], min = 0, max = 100, symbol = '%', gaugeSectors(
                success = c(80, 100), warning = c(40,79), danger = c(0, 39)
              ))#gauge
            })

            linkcounts <- nnupdate$Linkcounts
            output$linkcounts <- render_gt({
              linkcounts %>%
                mutate(`Values`=scales::comma(as.numeric(`Values`))) %>%
                gt(rownames_to_stub=TRUE,rowname_col="") %>%
                tab_header(title = "Linkcounts",
                           subtitle = "Key Diifferences in Observations") %>%
                #tab_source_note(md("drscore")) %>%
                cols_align(
                  align = c("right"),
                  columns = c(`Values`)
                )
            })

            output$distinct <-  renderValueBox({
              bs4Dash::valueBox(
                subtitle= "Proportion Distinct",
                value=scales::percent(linkscore[,1][3]),
                icon = icon("percent"),
                width = 12
              )
            })

            output$estimated <-  renderValueBox({
              bs4Dash::valueBox(
                subtitle="Proportion Estimated",
                value=scales::percent(linkscore[,1][4]),
                icon = icon("percent"),
                width = 12
              )
            })

            output$undeniable <-  renderValueBox({
              bs4Dash::valueBox(
                value=scales::percent(linkscore[,1][5]),
                subtitle ="Proportion Undeniable",
                icon = icon("percent"),
                width = 12
              )
            })

            LinkScore_Levels <- nnupdate$LinkScore_Levels %>%
              mutate_if(is.numeric, round,2)
            output$LinkScore_Levels <- renderDataTable({
              LinkScore_Levels %>% datatable(escape = FALSE, class = 'cell-border stripe',
                                             caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Category Level Disclosure Risk: '),
                                             rownames = TRUE,
                                             options = list(searching = TRUE,pageLength = 10,scrollY=TRUE, scrollX = TRUE))
            })

            output$linkcountd <- downloadHandler(
              filename=function() {
                paste("linkcounts-", Sys.Date(), ".csv", sep="")
              },
              content = function(file){
                write.csv(linkcounts,file)
              }
            )

            output$linkscored <- downloadHandler(
              filename=function() {
                paste("linkscores-", Sys.Date(), ".csv", sep="")
              },
              content = function(file){
                write.csv(linkscore,file)
              }
            )

            output$LinkScore_Levelsd <- downloadHandler(
              filename=function() {
                paste("LinkScore_Levels-", Sys.Date(), ".csv", sep="")
              },
              content = function(file){
                write.csv(LinkScore_Levels,file)
              }
            )


          } #else

        }) #observeEvent update
      } #else


    } #else




  }) #observeEvent drscore


    observeEvent(input$update, {

      if(input$drscore==0){
        shinyalert(title = "Get the drscore!",
                   text = "We couldn't find the DRisk Object, please try drscore")
      }

      })





  output$hline <- renderUI({

    tagList(
      tags$br(),
      tags$hr(),
      tags$br())
  })



  output$sample <-  renderDataTable({
    file <- input$sample
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.csv(file$datapath, TRUE) %>% datatable(escape = FALSE, class = 'cell-border stripe',
                             caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Sample Data'),
                             rownames = TRUE,
                             options = list(searching = TRUE,pageLength = 25,scrollY=TRUE, scrollX = TRUE))
  })



  output$protected <-  renderDataTable({
    file2 <- input$protected
    ext <- tools::file_ext(file2$datapath)

    req(file2)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.csv(file2$datapath) %>% datatable(escape = FALSE, class = 'cell-border stripe',
                                                                   caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Protected Data'),
                                                                   rownames = TRUE,
                                                                   options = list(searching = TRUE,pageLength = 25,scrollY=TRUE, scrollX = TRUE))
  })

  CASC_sample <- read.csv(here::here("data/CASC_sample.csv"))
  dataModal <- function(failed = FALSE) {
    modalDialog(
      renderTable(head(CASC_sample),rownames=TRUE),
      easyClose=TRUE,
      footer = tagList(
        modalButton("Close")
      )
    )
  }

  # Show modal when button is clicked.
  observeEvent(input$show, {
    showModal(dataModal())
  })

  CASC_protected <- read.csv(here::here("data/CASC_protected.csv"))
  dataModalx <- function(failed = FALSE) {
    modalDialog(
      renderTable(head(CASC_protected),rownames=TRUE),
      easyClose=TRUE,
      footer = tagList(
        modalButton("Close")
      )
    )
  }

  # Show modal when button is clicked.
  observeEvent(input$show1, {
    showModal(dataModalx())
  })

  # Example
  observeEvent(input$rune, {
    nneg <- drscore(Sample = CASC_sample,
                  Protected = CASC_protected,
                  delta = 0.05,
                  kdistinct = 0.05,
                  ldeniable = 5,
                  neighbourhood = 1,
                  neigh_type = 'prob',
                  numeric.vars = 1:4,
                  outlier.par = list(centre = median,
                                     scale = var,
                                     thresh = 0.01))
    linkscore <- nneg$Linkscore %>%
      mutate(InvValues=(1-Values)*100)
    output$risksample <- renderGauge({
      gauge(round(linkscore[,2][1],2), min = 0, max = 100, symbol = '%', gaugeSectors(
        success = c(80, 100), warning = c(40,79), danger = c(0, 39)
      ))#gauge
    })

    output$riskoutlier <- renderGauge({
      gauge(round(linkscore[,2][2],2), min = 0, max = 100, symbol = '%', gaugeSectors(
        success = c(80, 100), warning = c(40,79), danger = c(0, 39)
      ))#gauge
    })


    linkcounts <- nneg$Linkcounts
    output$linkcounts <- render_gt({

      linkcounts %>%
        mutate(`Values`=scales::comma(as.numeric(`Values`))) %>%
        gt(rownames_to_stub=TRUE,rowname_col="") %>%
        tab_header(title = "Linkcounts",
                   subtitle = "Key Diifferences in Observations") %>%
        #tab_source_note(md("drscore")) %>%
        cols_align(
          align = c("right"),
          columns = c(`Values`)
        )
    })

    output$distinct <-  renderValueBox({
      bs4Dash::valueBox(
        subtitle= "Proportion Distinct",
        value=scales::percent(linkscore[,1][3]),
        icon = icon("percent"),
        width = 12
      )
    })

    output$estimated <-  renderValueBox({
      bs4Dash::valueBox(
        subtitle="Proportion Estimated",
        value=scales::percent(linkscore[,1][4]),
        icon = icon("percent"),
        width = 12
      )
    })

    output$undeniable <-  renderValueBox({
      bs4Dash::valueBox(
        value=scales::percent(linkscore[,1][5]),
        subtitle="Proportion Undeniable",
        icon = icon("percent"),
        width = 12
      )
    })

    LinkScore_Levels <- nneg$LinkScore_Levels %>%
      mutate_if(is.numeric, round,2)
    output$LinkScore_Levels <- renderDataTable({

      LinkScore_Levels %>% datatable(escape = FALSE, class = 'cell-border stripe',
                               caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Category Level Disclosure Risk: '),
                               rownames = TRUE,
                               options = list(searching = TRUE,pageLength = 10,scrollY=TRUE, scrollX = TRUE))
    })

    output$linkcountd <- downloadHandler(
      filename=function() {
        paste("linkcounts-", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(linkcounts,file)
      }
    )

    output$linkscored <- downloadHandler(
      filename=function() {
        paste("linkscores-", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(linkscore,file)
      }
    )

    output$LinkScore_Levelsd <- downloadHandler(
      filename=function() {
        paste("LinkScore_Levels-", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(LinkScore_Levels,file)
      }
    )



    output$sample <-  renderDataTable({
      CASC_sample %>% datatable(escape = FALSE, class = 'cell-border stripe',
                    caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Example: CASC Sample Data'),
                    rownames = TRUE,
                    options = list(searching = TRUE,pageLength = 25,scrollY=TRUE, scrollX = TRUE))
    })

    output$protected <-  renderDataTable({
      CASC_protected %>% datatable(escape = FALSE, class = 'cell-border stripe',
                    caption = htmltools::tags$caption (style = 'caption-side: top; text-align: center;
                                                              color: black; font-family: Arial;
                                                              font-size: 150% ;', 'Example: CASC Protected Data'),
                    rownames = TRUE,
                    options = list(searching = TRUE,pageLength = 25,scrollY=TRUE, scrollX = TRUE))
    })

    }) #example





  # About


  url <- a("Our world in Data", href="https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita")

  world <- a("World Bank", href="https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita")
  output$about <- renderUI({

    tagList(tags$h1("About"),
            tags$hr(),
            tags$h2("Clean Fuels and Cooking"),
            tags$img(height = 300 , width = 812, src = "https://c.ndtvimg.com/2020-05/tpj5o4f8_cooking-_625x300_02_May_20.jpg"),
            tags$br(),
            tags$p("The data here represents the access of the clean cooking energy for the households in the countries of the world. Additionally, there is information for GDP per capita and popularion across 2000 and 2016 for all countries. This app examines the trends in these factors. We have plotted relationship of GDP per capita and access to clean fuel as a percentage in a scatter plot. The user can slide through the year slide to view the transition across different years. The linearise box flattens the trend using logarithmic translation and the hide low population countries hides countries with population less than one million in that year. The table presents these figures with the relative and absolute changes for all factors for each slider input year. You can also select the countries to see the trend within these selected countries."),
            tags$p("This is shinyApp, generated by RStudio.
                       The data used for the visualisation is acquired from "), url,
            tags$br(),
            tags$p("And the original dataset is borrowed from "), world,
            tags$hr(),
            tags$hr(),
            tags$h2("About me"),
            tags$br(),
            tags$p("I am Mohammed Faizan, currently studying Master of Business Analytics at Monash University. I have completed my first semester and I am so excited about this field of statistical theories and data analysis. I am a true believer in the power of mathematics, that is underlying to all of our approaches in analytics.

Being ambitious about life motivates me as a person with constant and ingenious efforts. With a high grasping power and acquired knowledge with it, I desire to be placed in challenging tasks. I am a charismatic individual encouraging everyone around to be happy and to grow to their highest potential. Researching, working and applying data analytics for optimising our environment in terms of climate change, energy usage and food security is my imperative objective. "),
            tags$hr(),
            tags$hr())

  })
}


