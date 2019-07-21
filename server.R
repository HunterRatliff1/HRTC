## ---- GLOBALS ----
# These are global, so should go in BOTH the ui.R and server.R files 
library(shinybootstrap2) # You probably do not want to attach this package (with library() or require()).
library(shiny)           #   Instead, you should use shinybootstrap2::withBootstrap2().
library(tidyverse)
library(ggthemes)
library(DT)
library(scales)
library(GGally)
library(survival)
library(survminer)

# suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(plotly))
library(waffle)



## ---- * Import data ----
# Read in the master df from csv
# masterDf <- suppressMessages(read_csv("Data/MergedDf.csv"))
masterDf <- readRDS("Data/MergedDf2.RDS") %>%
  mutate(BaseMsg=str_wrap(BaseMsg, width=60)) # Wrap the base message so it's easier to read

# Import the users data
users <- readRDS("Data/users.RDS")
  

# Create lists of demographic opts for filtering
optsGender <- as.character(count(masterDf, gender)$gender) # Consider doing a list to translate
optsGrade  <- as.character(count(masterDf, grade)$grade)   # abbrevs to full words, like on the
optsRace   <- as.character(count(masterDf, race)$race)     # HPO shiny app
optsQtype  <- as.character(count(masterDf, Type)$Type) 
#    --- end(globals) ---

## ---- Shiny server ----
shinyServer(function(input, output) {

  ## -------------------------- ##
  ##  Conditional UI filter   ---- 
  ## -------------------------- ##
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
  
  
  
  
  ## -------------------------- ##
  ## >       REACTIVES        < ## ---- 
  ## -------------------------- ##
  
  # ---- sidebarFilter() fxn ----
  sidebarFilter <- reactive({
    # Set the default to the masterDf (unfiltered)
    df <- masterDf %>% 
      
      filter(Type %in% na_if(input$qtype, "NA")) 
    
    # For each category to filter by (e.g. Gender, Grade), redefine df based on
    # results. Because these are all in the same reactive(), if the user changes
    # the category, it re-runs from the top, clearing previous filters. This 
    # prevents them from filtering to groups that are too small
    # > Use na_if() to convert "NA" to <NA> value
    if(input$filter_type=="Gender") df<-filter(df, gender %in% na_if(input$gender, "NA"))
    if(input$filter_type=="Grade") df<-filter(df, grade %in% na_if(input$grade, "NA")) 
    if(input$filter_type=="Race") df<-filter(df, race %in% na_if(input$race, "NA"))
    
    # return df
    df %>% 
      # Make cols of correct type
      mutate(
        Response = as.character(Response),
        CorAns   = as.character(CorAns),
        gender   = as.factor(gender),
        race     = as.factor(race),
        Type     = as.factor(Type),
        Category = as.factor(Category)
      ) %>%
      # Make cols for if they replied and if they got it right
      mutate(Replied = ifelse(is.na(Response), 0, 1)) %>%
      mutate(gotRight = ifelse(is.na(Response), NA,
                               ifelse(Response==CorAns,1,0))) %>%
      # Reorder the columns
      select(userID, gender, grade, race,  DateSent, 
             mid, Response, Replied, gotRight,
             CorAns, Type, Category, Keywords, BaseMsg, ReplyMsg, 
             everything())
    
    
  })
  
  ## Renders text of how many students included in sidebarFilter()
  output$numStudents <- renderText({
    df <- sidebarFilter()
    length(unique(df$userID))
  })
  
  # ---- msg() Filter msg data ----
  msg <- reactive({
    sidebarFilter() %>% # calls above function
      filter(BaseMsg==input$message) %>%
      select(-Message, -ReplyMsg, -BaseMsg)
    
  })
  
  # ---- msgAttr() ----
  # Attributes for all messages, based on sidebarFilter()
  msgAttr <- reactive({
    # Responded
    sidebarFilter() %>%
      
      group_by(mid, BaseMsg, Type, Category, Keywords, CorAns, campg.day) %>%
      
      summarise(
        gotRight = sum(gotRight, na.rm=T)/sum(Replied), # Only counts % gotRight if responded
        Replied = sum(Replied)/n()
      ) %>%
      ungroup()
  })
  
  # ---- hasAnswer() ----
  hasAnswer <- reactive({   # 
    sidebarFilter() %>%     # Might want to delete
      filter(!is.na(CorAns))   #
  })
  
  # ---- descriptive() ----
  descriptiveFilter <- reactive({
    #--- Save inputs for quick access
    groupVar <- input$descrSelVar
    filterNA <- input$descrIncNA
    activeSubscriber <- input$descrActive
    
    
    df <- users # Define base df as the users df
    
    #-- Filter by the checkboxes based on groupVar
    if(activeSubscriber) df <- df %>% filter(status.user=="Active Subscriber")
    if(filterNA) df <- df %>% filter(!is.na(!!sym(groupVar)))
    
    df #return df
  })
  
  
  # ---- descriptive2D() ----
  descriptive2D <- reactive({
    #--- Save inputs for quick access
    groupVarX <- input$descrSelX
    groupVarY <- input$descrSelY
    filterNA <- input$descrIncNA2 # different than the other one
    activeSubscriber <- input$descrActive2 # different than the other one
    
    
    df <- users # Define base df as the users df
    
    #-- Filter by the checkboxes based on groupVar
    if(activeSubscriber) df <- df %>% filter(status.user=="Active Subscriber")
    if(filterNA) df <- df %>% filter(!is.na(!!sym(groupVarX)), !is.na(!!sym(groupVarY)))
    
    df #return df
  })
  
  
  
  ## -------------------------- ##
  ## >     Descriptive Tab    < ## ---- 
  ## -------------------------- ##
  
  
  # ---- descrPlot1 ----
  ##            descrPlot1a
  output$descrPlot1a <- renderPlotly({
    groupVar <- input$descrSelVar
    
    # Set standard pals
    pal <- "Set2" # default
    if(groupVar=="gender") pal <- "Set1"
    if(groupVar=="grade") pal <- "Spectral"
    if(groupVar=="race") pal <- "Dark2"
    
    p1a <- descriptiveFilter () %>%
      group_by(!!sym(groupVar), add=T) %>% 
      ggplot(aes(x=!!sym(groupVar), fill=!!sym(groupVar))) + 
      geom_bar() + scale_fill_brewer(palette=pal, guide=F, na.value="grey50") +
      coord_flip() + theme_fivethirtyeight() +
      # guides(fill=guide_legend(title=NULL)) +
      theme(legend.position="none") 
    ggplotly(p1a)
  })
  
  
  ##            descrPlot1b
  output$descrPlot1b <- renderPlot({
    groupVar <- input$descrSelVar
    
    # Set standard pals
    numFactors <- dim(count(descriptiveFilter(), !!sym(groupVar)))[1]
    if(groupVar=="gender") pal <- suppressWarnings(RColorBrewer::brewer.pal(numFactors, "Set1"))
    if(groupVar=="grade") pal <- suppressWarnings(RColorBrewer::brewer.pal(numFactors, "Spectral"))
    if(groupVar=="race") pal <- suppressWarnings(RColorBrewer::brewer.pal(numFactors, "Dark2"))
    
    
    descriptiveFilter() %>%
      count(!!sym(groupVar)) %>% 
      waffle(legend_pos="bottom", flip=T, colors=pal)
  })
  
  # ---- descrPlot2 ----
  ##            descrPlot2a 
  output$descrPlot2a <- renderPlotly({
    groupVar <- input$descrSelVar
    
    # Set standard pals
    pal <- "Set2" # default
    if(groupVar=="gender") pal <- "Set1"
    if(groupVar=="grade") pal <- "Spectral"
    if(groupVar=="race") pal <- "Dark2"
    
    p2a <- descriptiveFilter() %>%
      group_by(userID) %>%
      group_by(!!sym(groupVar), add=T) %>% # dynamically add in 
      count(status.user) %>%
      ggplot(aes(x=status.user, fill=!!sym(groupVar))) + 
      geom_bar(position = "dodge") +
      coord_flip() + theme_fivethirtyeight() +
      # guides(fill=guide_legend(title=NULL)) +
      scale_fill_brewer(palette=pal, na.value="grey50")
      
    ggplotly(p2a)
  })
  
  ##            descrPlot2b 
  output$descrPlot2b <- renderPlotly({
    groupVar <- input$descrSelVar
    
    p2b <- descriptiveFilter() %>%
      group_by(userID) %>%
      group_by(!!sym(groupVar), add=T) %>% # dynamically add in
      count(status.user) %>%
      ggplot(aes(!!sym(groupVar), fill=status.user)) +
      geom_bar(position = "fill") +
      coord_flip() + theme_fivethirtyeight() +
      scale_y_continuous(limits=c(0,1), labels = scales::percent) +
      scale_fill_manual(name="User status",
        values = c("Undeliverable / Opt out"="#df382c", "Active Subscriber"="#38b44a"))
    
    ggplotly(p2b)
  })
  
  # ---- descrTable (DT) ----
  output$descrTable <- DT::renderDataTable({
    descriptive2D() %>%  
      count(!!sym(input$descrSelX), !!sym(input$descrSelY)) %>%
      spread(!!sym(input$descrSelX), n) %>%
      DT::datatable(rownames=F) 
  })
  
  # ---- descrHeat ----
  output$descrHeat <- renderPlotly({
    groupVarX <- input$descrSelX
    groupVarY <- input$descrSelY
    
    p <- descriptive2D() %>%  
      count(!!sym(groupVarX), !!sym(groupVarY)) %>%
      ggplot(aes(x=!!sym(groupVarX), y=!!sym(groupVarY), fill=n)) +
      geom_tile(color="black") +
      scale_fill_continuous_tableau(name="# subscribers") +
      coord_fixed() +
      theme_fivethirtyeight()
    ggplotly(p)
  })
  
  
  
  ## -------------------------- ##
  ## >      haveAns Tab       < ## ---- 
  ## -------------------------- ##
  
  # ---- CorrectFacet ----
  output$CorrectFacet <- renderPlotly({
    df <- sidebarFilter()
    p <-df %>%
      mutate(None = "None") %>% # Make this col it doesn't break if None selected
      filter(!is.na(CorAns)) %>% # Only include questions with correct answers
      group_by(!!rlang::sym(input$CorrectFacet_input)) %>% # dynamically add in 
      group_by(mid, BaseMsg, Type, Category, Keywords, CorAns, add=T) %>%
      
      summarise(
        gotRight = sum(gotRight, na.rm=T)/sum(Replied), # Only counts % gotRight if responded
        Replied = sum(Replied)/n()
      ) %>%
      ungroup() %>%
      arrange(gotRight) %>%
      rename(`Base Message`=BaseMsg) %>%
      ggplot(aes(x=mid, y=gotRight, 
                 fill=Replied, # fill=!!rlang::sym(input$CorrectFacet_input),
                 text=`Base Message`)) +
      geom_col(color="black", position = "dodge") +
      coord_flip() +
      labs(title="Percent who got correct", y="", x="Message ID") +
      scale_y_continuous(limits=c(0,1), labels = scales::percent) +
      scale_fill_continuous_tableau("Area Green",  labels = scales::percent)
    if(input$CorrectFacet_input!="None") p <- p + facet_wrap(input$CorrectFacet_input, scales = "free_y")
    
    ggplotly(p) 
  })
  
  # ---- questionOverview (DT) ----
  output$questionOverview <- DT::renderDataTable({
    df <- msgAttr()
    
    df %>%
      filter(!is.na(CorAns)) %>% # Only include questions with correct answers
      select(Replied, gotRight, Type, Category, Keywords, BaseMsg) %>%
      ungroup() %>%
      rename(`Got Right`=gotRight, `Base Message`=BaseMsg) %>%
      arrange(`Got Right`) %>%
      datatable(rownames=F, filter="top") %>%
      formatPercentage(c("Replied","Got Right")) %>%
      formatStyle("Replied", background=styleColorBar(c(0,1),'lightblue')) %>%
      formatStyle("Got Right", background=styleColorBar(c(0,1), 'lightgreen'))
  })
  
  
  
  ## -------------------------- ##
  ## >     QUESTION Tab       < ## ---- 
  ## -------------------------- ##
  
  
  
  # ---- 'message' UI ----
  output$message <- renderUI({
    
    df <- sidebarFilter()
    availableMessages <- count(df, BaseMsg)$BaseMsg
      
    selectInput('message', 'Message',  # use base message = mid
                choices=availableMessages)
  })

  # ---- Msg reply (UI) ----
  
  # Render reply msg (if available) 
  # Use `renderUI` & `htmlOutput` instead of `renderText` & `textOutput`
  output$questionReply <- renderUI({
    tryCatch(
      HTML(str_replace_all( # Print as HTML to include newlines
        unique(filter(masterDf, BaseMsg==input$message)$ReplyMsg)[1],
        "\r\n", "<br/>")), 
      finally = " ") # not working if error
  })
  
  # ---- msgAttributes (UI) ----
  output$msgAttributes <- renderUI({
    df <- msgAttr() %>%
      filter(BaseMsg==input$message)
    
    HTML(paste0("<b>Percent replied:</b> ", percent(df$Replied), "<br>",
                "<b>Percent correct:</b> ", percent(df$gotRight), "<br>",
                "<hr>",
                "<b>Message Type:</b> ", df$Type, "<br>", 
                "<b>Category:</b> ", df$Category, "<br>",
                "<b>Keywords:</b> ", df$Keywords, "<br>"
    ))
  })
  
  
  # ---- qResponsePlot ----
  output$qResponsePlot <- renderPlotly({ # plot
    
    df<- msg()
    mid <- count(df, mid)$mid

    # If box is not selected, filter out NAs
    if(!input$qResponsePlot_incNA) df<-filter(df,!is.na(Response))
    
    p<-df %>% 
      mutate(
        Response=str_trunc(as.character(Response), width=50),
        `Correct?` = ifelse(is.na(CorAns), 
                            ifelse(!is.na(Response), "Responded", "No response"),
                            ifelse(gotRight==1, "Correct","Incorrect"))
        ) %>%
      
      ggplot(aes(x=Response, fill=`Correct?`)) +
      geom_bar(color="black") + 
      scale_fill_manual(na.value="grey50", 
                        values = c("Correct"="#008c48", "Incorrect"="#EE2E2F", 
                                   "Responded"="#185AA9", "No response"="grey50")) +
      coord_flip() + 
      labs(title=paste0("Responses for mid=", mid))
         
      
    ## Conditional faceting
    # More info: https://shiny.rstudio.com/gallery/conditionalpanel-demo.html
    # More info: https://plot.ly/r/shinyapp-explore-diamonds/
    facetVar <- input$qResponsePlot_facet
    if (facetVar != '.') p <- p + facet_wrap(facetVar)
    
    # if (facetVar != '.') p <- p + facet_grid(paste0(". ~ ",facetVar))
    ggplotly(p)
    
    
  })
  # ---- qResponseTable (DT) ----
  output$qResponseTable <- DT::renderDataTable({ # table
    
    msg() %>% 
      mutate(Response=as.character(Response)) %>%
      
      count(Response) %>%
      mutate(Response = ifelse(is.na(Response), "No Response", Response)) %>%
      mutate(`%`=n/sum(n)) %>%
      datatable(rownames=F) %>%
      formatPercentage(c("%")) %>%
      formatStyle("%", background=styleColorBar(c(0,1),'lightgrey')) 
    # formattable( list()) # list(~ percent) formats all cols as percentages
    
    # datatable(rownames=F) %>%
    #   formatPercentage(c("Replied","Got Right")) %>%
    #   formatStyle("Replied", background=styleColorBar(c(0,1),'lightblue')) %>%
    #   formatStyle("Got Right", background=styleColorBar(c(0,1), 'lightgreen'))
    
  })
  
  
  
  
  
  
  ## -------------------------- ##
  ## >    RESPONSE RATE Tab   < ## ---- 
  ## -------------------------- ##
  
  # ---- respQuestOverall ----
  output$respQuestOverall <- renderPlotly({
    p <- sidebarFilter() %>%
      mutate(Type=ifelse(is.na(Response), NA, as.character(Type))) %>%
      ggplot(aes(x=mid, fill=Type, text=BaseMsg)) +
      geom_bar(aes(n=stat(count)), position="fill") + 
      scale_y_continuous(limits=c(0,1), labels = scales::percent) +
      labs(title="Overview of responses by question", 
           y="% No response (grey) / Replied (color)", x="") +
      coord_flip() 
      
    ggplotly(p)
  })
  
  # ---- respQuestTime ----
  output$respQuestTime <- renderPlotly({
    df <- sidebarFilter() %>%
      mutate(Replied = ifelse(is.na(Response), 0, 1)) %>%
      group_by(mid, BaseMsg, Type, Category, Keywords, CorAns, campg.day) %>%
      mutate(Replied = sum(Replied)/n()) %>%
      mutate(gotRight = ifelse(as.character(Response)==as.character(CorAns),
                               1,0)) %>%
      summarise(
        Replied = mean(Replied),
        gotRight = sum(gotRight)/n()
      ) %>% 
      ungroup()
    
    p <- df %>%
      ggplot(aes(x=campg.day, y=Replied, text=BaseMsg))
    # Add geoms based on check boxes
    if(!("Bars" %in% input$respQuestTime_boxes)) p <- p +
      geom_point(aes(color=Type, group=Type)) # only show points when !Bars
    if("Lines" %in% input$respQuestTime_boxes) p <- p +
      geom_line(aes(color=Type, group=Type), alpha=0.5)
    if("Bars" %in% input$respQuestTime_boxes) p <- p +
      geom_col(aes(fill=Type), color="black")
    
    # Finsih plot and render
    p <- p +
      labs(y="Percent Replied", x="Date Message Sent") + 
      scale_y_continuous(limits=c(0,max(df$Replied, na.rm=T)), labels = scales::percent)
    ggplotly(p) 
  })
  
  # ---- respUserSurv ----
  output$respUserSurv <- renderPlotly({
    
    groupVar <- input$respUserSurv_input
    # Set standard pals
    pal <- "Set2" # default
    if(groupVar=="gender") pal <- "Set1"
    if(groupVar=="grade") pal <- "Spectral"
    if(groupVar=="race") pal <- "Dark2"
    
    # Make Surv object
    survObj <- Surv(time=users$SurvEnd, event=users$Censor) # not including any event data
    formulaInput <- as.formula(paste0("survObj ~ ", groupVar))
    
    # Calculate the p value (if group by != 1)
    if(groupVar!="1") {
      survDif <- survdiff(formulaInput, data = users)
      df <- length(survDif$n)-1
      pvalue <- round(pchisq(survDif$chisq, df=df, lower.tail = F), 4)
    }
    
    # Make ggplot
    p <- survfit(formulaInput, data = users) %>%
      ggsurv() + 
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(limits=c(0,input$respUserSurv_slider),
                         name="Days into campaign")
    
    # If p value, add it in
    if(groupVar!="1") p<-p + 
      geom_text(aes(label=paste0("p-value: ",pvalue)),
                x=input$respUserSurv_slider/2, y=0.97)
    
    # if grouped, set the palette
    if(groupVar!="1") p<-p + 
      scale_color_brewer(palette=pal, na.value = "grey50")
      
    
      
    
    ggplotly(p)
    # #Code to do the ggsurvplot
    # survfit(formulaInput, data = users) %>%
    #   ggsurvplot(data = users, pval=T, #conf.int=T,
    #              risk.table = TRUE,
    #              fun = "pct",
    #              size = 1,
    #              legend = "bottom")
  })
  
  # ---- respUserTime ----
  output$respUserTime <- renderPlotly({
    inputVar <- input$respUserTime_input
    # Set standard pals
    pal <- "Set2" # default
    if(inputVar=="gender") pal <- "Set1"
    if(inputVar=="grade") pal <- "Spectral"
    if(inputVar=="race") pal <- "Dark2"
    
    
    p<- sidebarFilter() %>%
      mutate(grade=as.factor(grade)) %>%
      filter(!is.na(campg.day)) %>%
      mutate(Replied=ifelse(is.na(Response),F,T)) %>%
      group_by(userID) %>%
      arrange(campg.day) %>%
      mutate(numReplies    = cumsum(Replied)) %>%
      mutate(maxNumReplies = max(numReplies)) %>%
      mutate(numReplies = ifelse(is.na(Response) & numReplies==maxNumReplies,
                                 NA,numReplies)) %>%
      mutate(numReplies2 = ifelse(is.na(Response) & numReplies==lag(numReplies),
                                 NA,numReplies)) %>%
      ungroup() %>%
      ggplot(aes_string(x="campg.day", y="numReplies", color=inputVar)) +
      scale_color_brewer(palette = pal, na.value = "grey50")
    
    # If box checked, add detailed lines & points
    if(input$respUserTime_box) p<-p + 
      geom_path(aes(group=userID), size=0.3, alpha=0.25, # linetype = 5,
                position=position_jitterdodge(jitter.width=0.4, jitter.height=0.2, seed=2)) +
      geom_point(aes(y=numReplies2, group=userID), alpha=0.25, 
                 position=position_jitterdodge(jitter.width=0.4, jitter.height=0.2, seed=2))
    
    # Add in the rest of the plot and render
    p <- p + geom_smooth(aes_string(group=inputVar), method="auto") +
      labs(x="Date Message Sent", y="Cumulative Responses", title="User's Responses vs Time")
    ggplotly(p)
  })
  
  
  # ---- respUserHist ----
  output$respUserHist <- renderPlot({
    sidebarFilter() %>%
      filter(!is.na(CorAns)) %>% # ignore messages with no correct answer
      mutate(
        Response=as.character(Response),
        Response = ifelse(is.na(Response), "NoReply",
                            ifelse(Response==as.character(CorAns), 
                                   "Correct","Incorrect"))
      ) %>%
      group_by(userID, Response) %>%
      summarise(n=n()) %>%
      mutate(Percent=n/sum(n)) %>%
      # spread(Response, n) %>%
      # gather(ResponseType, Response, -userID, -NoReply) %>%
      arrange(userID) %>%
      filter(Response=="NoReply") %>%
      ggplot(aes(x=1-Percent)) + 
      geom_histogram(alpha=0.5, bins=20, color="black") +
      theme_fivethirtyeight() + theme(axis.title=element_text()) +
      scale_x_continuous(labels = percent) + 
      labs(title="Percent of messages responded to", 
           subtitle="Among messages that ask for a response",
           x="Percent of messages with response",
           y="Number of students")
    
  })
  
  
  
})