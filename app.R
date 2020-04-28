library(shiny)
library(tidyverse)
library(countdown)
library(beepr)
library(lubridate)

# Most of this app is directly from:
# https://stackoverflow.com/questions/49250167/how-to-create-a-countdown-timer-in-shiny

sounds = tibble(
    sound = 1:11,
    choices = c("ping",
                "coin",
                "fanfare",
                "complete",
                "treasure",
                "ready",
                "shotgun",
                "mario",
                "wilhelm",
                "facebook",
                "sword")
)

ui <- fluidPage(
    hr(),
    titlePanel(
        h1('Timer with Sound')
    ),
    sidebarLayout(
        sidebarPanel(
            tableOutput('tbl'),
            selectInput("sound", "Choose a sound", choices = sounds$sound),
            actionButton('start','Start'),
            actionButton('stop','Stop'),
            actionButton('reset','Reset')
        ),
        mainPanel(
            numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1),
            textOutput('timeleft'),
            tags$head(tags$style("#timeleft{color: green;
                                 font-size: 50px;
                                 font-style: italic;
                                 }"
            )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # Initialize the timer, 10 seconds, not active.
    timer <- reactiveVal(10)
    active <- reactiveVal(FALSE)
    sound_to_play <- reactiveVal(8)
    
    
    # Output the time left.
    output$timeleft <- renderText({
        paste("Time left: ", seconds_to_period(timer()))
    })
    
    # observer that invalidates every second. If timer is active, decrease by one.
    observe({
        invalidateLater(1000, session)
        isolate({
            if(active())
            {
                timer(timer()-1)
                if(timer()<1)
                {
                    active(FALSE)
                    beep(sound_to_play())
                    # showModal(modalDialog(
                    #     title = "Important message",
                    #     beep(7)
                    # ))
                }
            }
        })
    })
    
    # observers for actionbuttons
    observeEvent(input$start, {active(TRUE)})
    observeEvent(input$stop, {active(FALSE)})
    observeEvent(input$reset, {timer(input$seconds)})
    observeEvent(input$sound, {sound_to_play(as.numeric(input$sound))})
    
    output$tbl <- renderTable({ sounds}) 
}

shinyApp(ui, server)