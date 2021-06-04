
library(shiny)

fileUploadUI <- function(id, title_text){
    ns <- NS(id)
    tagList(
        fileInput(ns("raw_stats"), 
                      label=title_text,
                      accept = c('text/xlsx',
                                 '.xlsx'))
            )
}

fileUpload <- function(input, output, session){
    output$ui_output <- renderUI({
        input$raw_stats
    })
}
sliderTextUI <- function(id){ # add an id argument
    ns <- NS(id) # make a namespace function
    tagList( # wrap all input and outputs ids in "ns"
        sliderInput("slider", 1,10,2,0.2),
        textOutput(ns("number"))
    )
}

sliderText <- function(input, output, session){ # must use session for module servers
    output$number <- renderText({
        input$slider
    })
    
}

ui <- fluidPage(
    radioButtons("analysis_selection", "Select Analysis Type", choices = c("Manual", "OMIQ"), inline=TRUE),
    sliderTextUI("one"),
    sliderTextUI("two"),
    fileUploadUI("raw_stats_1", "Manual"),
    fileUploadUI("raw_stats_1", "OMIQ")
)

server <- function(input, output, session) {
    # callModule(sliderText, "one")
    # callModule(sliderText, "two")
    if(input$analysis_selection == "Manual"){
        callModule(fileUpload, "raw_stats_1")
    }
    else{
        callModule(fileUpload, "raw_stats_2")
    }
   
    
}

shinyApp(ui, server)

