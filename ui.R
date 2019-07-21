# ---- GLOBALS ----
## These are global, so should go in BOTH the ui.R and server.R files 
library(shinybootstrap2) # You probably do not want to attach this package (with library() or require()).
library(shiny)           #   Instead, you should use shinybootstrap2::withBootstrap2().
library(tidyverse)
library(ggthemes)
library(DT)
library(scales)
# suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(plotly))



library(DT)
suppressPackageStartupMessages(library(plotly))

## Import data 
masterDf <- readRDS("Data/MergedDf2.RDS") %>%
  mutate(BaseMsg=str_wrap(BaseMsg, width=60))

# Import the users data
users <- readRDS("Data/users.RDS")

# Create lists of demographic opts for filtering
optsGender <- as.character(count(masterDf, gender)$gender) # Consider doing a list to translate
optsGrade  <- as.character(count(masterDf, grade)$grade)   # abbrevs to full words, like on the
optsRace   <- as.character(count(masterDf, race)$race)     # HPO shiny app
optsQtype  <- as.character(count(masterDf, Type)$Type) 
#    --- end(globals) ---

## Shiny UI 
shinyUI(fluidPage(

  ## ---- *** Sidebar ----
  sidebarLayout(
    
    shiny::sidebarPanel(width=3,
      
      h4("Filter options"), 
        selectInput("qtype", "Question Type", multiple=T,
                    choices=optsQtype, selected=optsQtype), 
      # Filter options ----
      hr(),
        selectInput("filter_type", "Filter by:",    # Select filter type
                    c("Gender", "Grade", "Race")),  
        uiOutput("filter_by"),                     # dependent ui element (from above)
      p(em("Displaying results of ", code(textOutput("numStudents",inline=T)), " of ", 
           strong(length(unique(masterDf$userID)))," users"))
    ), # end(sidebar)
    
    ## *** Main panel ----
    shiny::mainPanel(width=9,
      shiny::navbarPage("HRTC", 
        
        ## ---- >> Descriptive ----                
        tabPanel(
          "Descriptive",
          h3("Descriptive statistics"),
          
          ## descriptive inputs ----
          div(class="well-sm bg-success", #start well
            fluidRow(
              column(width=6, selectInput('descrSelVar', "Varible", 
                choices = c("Gender"="gender", "Grade"="grade", "Race"="race"))),
              column(width=6, 
                     shiny::checkboxInput('descrIncNA', label="Exclude NA?", value=T),
                     shiny::checkboxInput('descrActive', label="Only show active subscribers?", value=F))
            )
          ), #end(well)
          p("Here are some overview statistics of the composition of the people
            who have subscribed to the campaign. Use the options in the green boxes
            to toggle how the graphs/tables render"),
          
          div(class="well-lg bg-warning", strong("Note: "),"Unlike elsewhere in 
              this app, adjusting the values on the sidebar on the left do ",
              em(strong("not")), " filter the data on this page."),
          
          ## descrPlot1 (A & B) ----
          h3("Number of users"),
          fluidRow(
            column(width=8, plotlyOutput("descrPlot1a")),
            column(width=4, plotOutput("descrPlot1b"))
          ),
          
          ## descrPlot2 (A & B) ----
          h3("Numbers by user status"),
          plotlyOutput("descrPlot2a"),
          plotlyOutput("descrPlot2b"),
          
          ## descrTable & descrHeat ----
          h3("Table & Heatmap"),
          div(class="well-sm bg-success", 
            fluidRow(
              column(width=6, selectInput(
                'descrSelX', "Varible X", 
                choices = c("Grade"="grade", "Race"="race", "Gender"="gender"))),
              column(width=6, selectInput(
                'descrSelY', "Varible Y", 
                choices = c("Gender"="gender", "Grade"="grade", "Race"="race")))
            ),
            fluidRow(
              column(width=6, shiny::checkboxInput(
                'descrIncNA2', label="Exclude NA?", value=T)),
              column(width=6, shiny::checkboxInput(
                'descrActive2', label="Only show active subscribers?", value=F))
            )
          ),
          p(),
          DT::dataTableOutput("descrTable"),
          plotlyOutput("descrHeat"),
          
          
          p("work in progress")
                 
           
                 
        ),
                        
                        
        ## ---- >> haveAns ----
        tabPanel("Have Answer",
                 h3("Messages with a correct answer", tags$small("(Quiz, T/F)")),
                 div(class = "well-sm bg-success",
                  selectInput('CorrectFacet_input', 'Group by', 
                              c(None="None", "Category", Gender="gender",Race="race", Grade="grade",`Question Type`="Type"))),
                 ## CorrectFacet ----
                 p("In the graph below, the length of the bar along the x-axis shows the percentage of respondants
                   who got the question right. The width of the bar and the color it's shaded represent the degree
                   to which respondants replied to the message. The percent correct I calculated didn't consider
                   non-responses, so the thicker, darker bars (more responses) have a larger ", em("n"), 
                   "than the thin, light bars"),
                 plotlyOutput("CorrectFacet", height="550px"),
                 div(class="well-sm bg-info",strong("Hint:"), 
                     "Hover over the bars to see the text message associated with the ", code("mid")),
                 hr(),
                 ## questionOverview (DT) ----
                 DT::dataTableOutput("questionOverview")
        ),                  
                        
                        
        
        ## ---- >> Questions tab ----                
       tabPanel("Question Details",   
                
          ## Select message ----
          div(class="well",
            h3("Select Text Message"),
            fluidRow(
              column(width=8, uiOutput("message"),
                     p(htmlOutput("questionReply")) # Show question
              ),
              column(width=4,
                     p(htmlOutput("msgAttributes")) # Show attributes
              )
            ) #end(fluidRow)
          ), #end(well)
          

          # * Graph & table tabs ----
          tabsetPanel( # not to be confused with larger panels
            tabPanel("Graph",
              fluidRow(
                column(width=8,div(class = "well-sm bg-success",
                       selectInput("qResponsePlot_facet",
                                   "Use the dropdown below to select a variable to facet by:",
                                   c(None='.',Gender="gender",Grade="grade",
                                     Race="race")),
                       p("When faceting, keep in mind these are absolute numbers of responses, and ",
                         strong("not"), " scaled to each group's respective size")
                )),
                column(width=4,div(class = "well-sm bg-success",
                                   shiny::checkboxInput('qResponsePlot_incNA', 'Include non-responses')
                ))
              ),
             
             
             ## qResponsePlot ----
             plotlyOutput("qResponsePlot"),
             
             div(class="well-sm bg-danger",
                 p(strong("Note for future:")," Might want to take some more steps to
                   assure individual level data isn't being reveled."),
                 pre("Something along the lines of:
facetVar <- input$qResponsePlot_facet
df <- msg() %>% count(facetVar, Response)
                     
Check to see if df$n is smaller than some cutoff value, and if so redefine facetVar to `None`"))
             
            ),
            tabPanel("Table", 
              p(strong("Response:"), " The users response"),
              p(strong("n:"), " Number of users who provided that response"),
              p(strong("%:"), " Percentage of all responses (including non-responses)"),
              
              ## qResponseTable (DT) ----
              DT::dataTableOutput("qResponseTable")
            )
          )
      ), # end(tabPanel=Questions)
       
    
      ## ---- >> Response rate ----
      tabPanel("Response rate",
               
               
               h2("Responses by", span(class="text-danger", "questions")),
               # respQuestOverall ----
               p("The bar graph below shows the overall percent of users who responded 
                 (the colored portion of the bar) for each message. Bars are colored by the question 
                 type (e.g. ", em("Quiz, T/F"),") with non-responses coded as the grey", strong("NA"), "bars"),
               div(class="well-sm bg-info", strong("Hint:"), 
                   "Hover over the bars to see the text message associated with the ", code("mid"),
                   ", as well as additional info like the exact number of respondants"),
               plotlyOutput("respQuestOverall", height = "500px"),
               p("As expected, questions that prompt a response (e.g. Quiz) get more responses than
                 do not (e.g. Hotline). While this is a helpful graph for seeing the overall picture,
                 the response rate varies substantially dependant on the timing during the campaign
                 (see below)"),
               
               # respQuestTime ----
               h3("Response rate during campaign", tags$small("(broken down by question)")),
               p("Below shows the same data as above, but ploted over the ", em("Day Message Sent"), ".",
                 "The x-axis shows the number of days since the user signed up for the service.
                 You can use the check boxes below to toggle the way the plot is generated."),
               fluidRow( #start row
                 column(width=6, div(class="well-sm bg-success",
                        shiny::checkboxGroupInput(
                          'respQuestTime_boxes', label = "Plot:", inline = T,
                          choices = c("Bars", "Lines"), selected = c("Bars", "Lines")))),
                 column(width=6, div(class="well-sm bg-info", strong("Tip:"), 
                                     "Try clicking on the legand of the graph. Single clicks
                                      removes that category, while double clicking isolates
                                      the category"))
               ), #end(row)
               plotlyOutput("respQuestTime"),
               
               hr(),
               h2("Responses by", span(class="text-danger", "users")),
               
               # respUserSurv ----
               h3("Survival plot"),
               fluidRow( #start row
                 column(width=7,
                        p("The ", code("+"), "marks on the curve respresent (right) 
                          censored data. Uses Kaplan–Meier estimators"),
                        p(class="bg-danger", "** TODO: Add more info here **"),
                        div(class="well-lg bg-warning", strong("Note: "),
                            "This survival plot is not resposive to any filters 
                            applied via the sidebar on the left")
                 ),
                 column(width=5, div(class="well-sm bg-success",
                                     selectInput('respUserSurv_input', "Group by:",
                                                 choices=c("None"=1, "Gender"="gender", "Race"="race", "Grade level"="grade")),
                                     shiny::sliderInput("respUserSurv_slider", "Number of days to show", 
                                                        value=max(users$SurvEnd, na.rm=T),
                                                        min=0, max=max(users$SurvEnd, na.rm=T))
                 ))
               ),
               plotlyOutput("respUserSurv"),
               
               # respUserTime ----
               h3("Number of user's responses over campaign"),
               p("This plot has a lot going on (if you select the ", em("Show details?")," box), so let me 
                 explain what's going on."),
               p("The thicker, curved lines represent a regression of the replies, grouped by whichever 
                 variable is selected below, and the grey shaded area represents the 95% confidence interval.
                 However, it's not a validated model, and is intended for more illustrative than technical 
                 purpopses. For those interested, ", 
                 tags$a(href="https://ropensci.github.io/plotly/ggplot2/geom_smooth.html", "this shows the math"),
                 "behind how the curve and confidence interval is handled."),
               
               p(tags$u("If you select the ", em("Show details?")," box:"), "The small dots connected by 
                 thin lines follows each user's number of cumulative responses over time. So each line 
                 represents one user's history, and each dot represents a reply. In order to visualize the 
                 data better, the lines/dots are scattered away from their true location a bit because
                 otherwise they'd all overlap.", 
                 em("Because there's a lot of data here, it might be helpful to", 
                 strong("use the zooming functions located at the top right corner of the graph"),".")),
               
               div(class = "well-sm bg-success", fluidRow( #start div
                 column(width=6, 
                        selectInput('respUserTime_input',"Select variable to color by:",
                                    c(Gender="gender",Race="race", Grade="grade"))),
                 column(width=6, shiny::checkboxInput('respUserTime_box', "Show details?"))
               )), # end(div)
               
               plotlyOutput("respUserTime", height=500),
               
               # respUserHist ----
               h3("Response rate by user"),
               p("The histogram below only includes messages that we expect a
                 response to (e.g. a Quiz or T/F), and ignores messages where
                 responses are not asked for or required (e.g. Knowledge, 
                 Knowledge w/ Ex)"),
               
               plotOutput("respUserHist")
               
               
     ),
     
     ## ---- >> About ----
     tabPanel("About", includeHTML("about.html"))
    
    
    ) # end(navbar page)
  )) # end(sidebar)
)) # end(page)
