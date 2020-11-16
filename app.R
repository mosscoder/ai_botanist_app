library(shiny)
library(tidyverse)
library(stringr)

baseDir <- '~/mpgPostdoc/projects/plantID/gh_data/'
obsDir <- paste0(baseDir, 'observations/')
if(!dir.exists(obsDir)){dir.create(obsDir)}

full_grid <- read.csv(paste0(baseDir,'photo_layout.csv'))
layoutPts <- read.csv(paste0(baseDir,'pts_layout.csv'))

full_grid$ind <- str_pad(full_grid$ind, 4, pad = '0') 

plotLayout <- function(pset, objective){
  colnames(layoutPts)[3] <- objective
  pDat <- left_join(layoutPts %>% as.data.frame(),
                    full_grid %>% 
                      filter(photo_set == pset),
                    by = objective) 
  pDat[is.na(pDat)] <- 0
  
  ggplot(pDat, aes(x = x, y = y, label = ifelse(species != 0, paste(ind, toupper(species), month, sep='\n'), ''))) +
    geom_point(size = 20) +
    theme_void() +
    geom_label( fontface = "bold") +
    coord_cartesian(xlim = c(min(pDat$x) - 90, max(pDat$x) + 90),
                    ylim = c(min(pDat$y) - 36, max(pDat$y) + 36)) 
}


ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    verbatimTextOutput('file_check'),
    selectInput(
      "photo_set",
      label = "Photo Set",
      choices = unique(full_grid$photo_set)
    ),
    radioButtons(
      "objective",
      label = "Objective",
      choices = c('center_pos', 'corner_pos', 'random_pos')
    ),
    checkboxGroupInput('flower_check', 'Which flowering:', choices = NULL),
    actionButton("stamp_time", "Stamp time")
  ),
  mainPanel(plotOutput('plot'))
))

server <-  function(input, output, session) {
  
  observe({input$photo_set})
  
  observeEvent(input$photo_set, {
    updateRadioButtons(
      session,
      "objective",
      label = "Objective",
      choices =  c('center_pos', 'corner_pos', 'random_pos'),
      selected = 'center_pos'
    )
  })
  
  observe({
    x <- input$photo_set
    
    focalInds <- full_grid %>%
      filter(photo_set == x) %>%
      select(ind) %>%
      unlist()
    
    updateCheckboxGroupInput(session, "flower_check", "Which flowering:",
                             choices = paste(focalInds))
   
  })
  
  observeEvent(input$stamp_time,{
    format_set <- str_pad(input$photo_set, 4, pad = '0') 
    
    file_base <- paste0(format_set, '_', str_remove(input$objective, '_pos'), '.csv')
    file_full <- paste(obsDir, file_base)
    if(file.exists(file_full)){
      showModal(modalDialog(
        "Stamped already!"
      ))
    } else {
      stamped <- data.frame(photo_set = format_set,
                            objective = input$objective,
                            time = Sys.time(),
                            flower_inds = paste(input$flower_check, collapse = ' ')
      )
                            
      write.csv(stamped, file_full, row.names = FALSE)
    }
    
  })
  
  output$plot <- renderPlot({
    plotLayout(pset = input$photo_set,
               objective = input$objective)
    
   
  })
  
  existing_files <- reactive({
    input$stamp_time
    fs <- list.files(path = obsDir, pattern = str_pad(input$photo_set, 4, pad = '0') )
    if(length(fs) == 0) {
      'No files!'
    } else {
      paste(fs, collapse = '\n')
    }
    
  })
  
  output$file_check <- renderText({existing_files()})  
  
}

shinyApp(ui, server)