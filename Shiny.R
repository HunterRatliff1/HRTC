#    --- GLOBALS ---
## These are global, so should go in BOTH the ui.R and server.R files 
require(shinybootstrap2)
require(shiny)
require(tidyverse)

# Read in the master df from csv
masterDf <- read_csv("Data/MergedDf.csv")

# Create lists of demographic opts for filtering
optsGender <- count(masterDf, gender)$gender # Consider doing a list to translate
optsGrade  <- count(masterDf, grade)$grade   # abbrevs to full words, like on the
optsRace   <- count(masterDf, race)$race     # HPO shiny app
#    --- end(globals) ---

ui <- fluidPage(
  
  titlePanel("An app using an observe, reactive and render"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select Text Message"),
      selectInput('message', 'Message', 
                  choices=count(masterDf, Message)$Message),
      hr(),
      h4("Filter options"), # TODO
      
      
      selectInput("filter_type", "Filter by:",    # Select filter type
                  c("Gender", "Grade", "Race")),  
      uiOutput("filter_by")                     # dependent ui element (from above)
      
      # # Will want to make it where you can only filter by one at a time
      # # See: https://shiny.rstudio.com/gallery/dynamic-ui.html
      # selectInput('gender', 'Gender', multiple=T,
      #             choices=optsGender, selected=optsGender),
      # selectInput('grade', 'Grade', multiple=T,
      #             choices=optsGrade, selected=optsGrade),
      # selectInput('race', 'Race', multiple=T,
      #             choices=optsRace, selected=optsRace)
    ), # end sidebar panel
    
    mainPanel(
      h3("Question:"),
      div(
        h4(textOutput("questionResult")),
        p(textOutput("questionReply")),
        class="well"
      ),
      tabsetPanel(
        tabPanel("Table",tableOutput("mytable")),
        tabPanel("Responses",
                 p("hi"))
      )
      
      
    ) # end main panel
  )
)

server <- function(input, output, session) {
  require(tidyverse)
  
  # Read in the master df from csv
  masterDf <- read_csv("Data/MergedDf.csv")
  
  output$filter_by <- renderUI({
    if (is.null(input$filter_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$filter_type,

           "Gender"=selectInput('gender', 'Gender', multiple=T,
                                choices=optsGender, selected=optsGender),
           "Grade"= selectInput('grade', 'Grade', multiple=T,
                                choices=optsGrade, selected=optsGrade),
           "Race"=  selectInput('race', 'Race', multiple=T,
                                choices=optsRace, selected=optsRace)
    )
  })
  
  
  # Filter the data
  dat <- reactive({
    # Not working right now
    Gender <- ifelse(input$filter_type=="Gender",
                    input$gender,
                    optsGender)
    Grade <- ifelse(input$filter_type=="Grade",
                   input$grade,
                   optsGrade)
    Race <- ifelse(input$filter_type=="Race",
                   input$race,
                   optsRace)
    
    masterDf %>% 
      filter(gender %in% Gender) %>%
      filter(race %in% Race) %>%
      filter(grade %in% Grade) %>%
      filter(Message==input$message) %>%
      select(-Message, -ReplyMsg, -BaseMsg)

  })
  # msg <- reactive({
  #   input$message
  # })
  # # Since dat() is generated from a reactive that 
  # # is triggered by input$myslider this table will update
  # # any time that input$myslider updates
  output$mytable <- renderTable({
    # dat()
    dat() %>%
      head()
    
  })
  
  
  # Just restates the question
  output$questionResult <- renderText({input$message})
    
  # Renders the response, if available
  output$questionReply <- renderText({
      tryCatch(unique(filter(masterDf, Message==input$message)$ReplyMsg), 
               finally = " ") # not working if error
  })
  
  
}



shinyApp(ui = ui, server = server)
