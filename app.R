#### Libraries ####
library(rpart)
library(plotly)
library(echarts4r)
library(htmlwidgets)
library(htmltools)
library(shinyalert)
library(shinyWidgets)
library(waiter)
library(shiny)
library(bs4Dash)
library(tidyverse)
#### Variables ####
train <- read_csv("data/train.csv")[, -(1:2)] 

df.raw <- 
  read_csv("data/new.csv")

df.raw$id <- 1:nrow(df.raw)
df.names <- df.raw %>% names

#### Model ####

# train 
model.tree <- rpart(
  satisfaction ~ .,
  data = train, method = "class",
  control = rpart.control(cp = 0, maxdepth = 5)
)




##### Body #####
body <- 
  bs4Dash::dashboardBody(
    # First Layer
    fluidRow(
      column(
        width = 4,
        fluidRow(
          column(
            width = 6,
            uiOutput("vbox.kpi")
          ),
          column(
            width = 6,
            uiOutput("vbox.n")
          )
        )
      ),
      column(
        width = 8,
        box(
          title = "Predict Passenger's Reaction",
          elevation = 2,
          width = 12,
          status = "gray-dark",
          solidHeader = T,
          collapsed = T,
          fluidRow(
            column(
              width = 4,
              uiOutput("vbox.pred")
            ),
            column(
              width = 4,
              uiOutput("vbox.pred.char")
            ),
            column(
              width = 4,
              uiOutput("vbox.pred.cust")
            ),
            column(
              width = 12,
              fluidRow(
                actionBttn(
                  inputId = "go.profile",
                  label = "Describe Passanger",
                  icon = icon("user"),
                  status = "success",
                  style = "fill",
                  size = "sm"
                ),
                actionBttn(
                  inputId = "go.importance",
                  label = "Factors Importance",
                  style = "fill",
                  status = "success",
                  size = "sm"
                )
              )
            )
          )
        )
      )
    ),
    # Second Layer
    fluidRow(
      box(
        title = "Flight distance vs. delays",
        elevation = 2,
        width = 12,
        status = "gray-dark",
        solidHeader = T,
        plotlyOutput("plot.flightscatter", width = "100%"),
        sidebar = boxSidebar(
          startOpen = F,
          id = "id1",
          pickerInput(
            "pick3", "Pick Facet",
            choices = c("Departure Delay in Minutes", "Arrival Delay in Minutes"),
            selected = "Type of Travel",
            width = "100%",
            options = list(
              `selected-text-format`= "count",
              `count-selected-text` = "{0} feature(s) out of 2",
              "max-options-text" = "No more!",
              style = "color: white"
            ),
            multiple = F
          )
        )
      )
    ),
    #Third Layer
    fluidRow(
      column(
        width = 7,
        box(
          title = "Passenger characteristic",
          elevation = 2,
          width = 12,
          status = "gray-dark",
          solidHeader = T,
          plotlyOutput("plot.bars", height = "700px"),
          sidebar = boxSidebar(
            startOpen = F,
            id = "id2",
            pickerInput(
              "pick1", "Pick Facet",
              choices = c("Gender", "Customer Type", "Type of Travel", "Class"),
              selected = "Type of Travel",
              width = "100%",
              options = list(
                `selected-text-format`= "count",
                `count-selected-text` = "{0} feature(s) out of 2",
                "max-options-text" = "No more!",
                style = "color: white"
              ),
              multiple = F
            ),
            pickerInput(
              "pick2", "Pick x",
              choices = c("Gender", "Customer Type", "Type of Travel", "Class"),
              selected = "Class",
              width = "100%",
              options = list(
                `selected-text-format`= "count",
                `count-selected-text` = "{0} feature(s) out of 2",
                "max-options-text" = "No more!",
                style = "color: white"
              ),
              multiple = F
            )
          )
        )
      ),
      column(
        width = 5,
        box(
          title = "Flight characteristics",
          elevation = 2,
          width = 12,
          status = "gray-dark",
          solidHeader = T,
          echarts4rOutput("plot.all", height = "700px")
        )
      )
    )
  )

#### Sidebar ####
sidebar <- 
  bs4Dash::dashboardSidebar(
    
    status = "success", collapsed = T, elevation = 3, expandOnHover = T,
    minified = F, width = "25%",
    
    sidebarMenu(
      id = "mainmenu", 
      # What we got
      menuItem(text = "Filters",
       fluidRow(
         lapply(df.names[1:22], function(x) {
           if(!(x %in% c("Gender", "Customer Type", "Type of Travel", "Class"))) {
             sliderInput(
               paste0("f.", x), x,
               value = c(min(df.raw[[x]], na.rm = T), max(df.raw[[x]], na.rm = T)),
               min = min(df.raw[[x]], na.rm = T),
               max = max(df.raw[[x]], na.rm = T),
               round = T, ticks = F,
               width = "100%"
             )
           }
         })
       )
      )
    )
  )

#### Header ####
header <- 
  bs4Dash::dashboardHeader(
    
  )


server <- function(session, output, input) {
#### Modal Logic ####
  observeEvent(input$go.profile, {
    showModal(
      modalDialog(
        size = "l",
        title = "Passenger data",
        
        fluidRow(
          lapply(df.names[c(1:3, 5, 4, 6:22)], function(x) {
            if(x %in% c("Gender", "Customer Type", "Type of Travel", "Class")) {
              pickerInput(
                paste0("p.", x), x,
                choices = unique(df.f()[[x]]),
                width = "50%"
              )
            } else {
              sliderInput(
                paste0("p.", x), x,
                value = mean(df.f()[[x]], na.rm = T),
                min = min(df.f()[[x]], na.rm = T),
                max = max(df.f()[[x]], na.rm = T),
                round = T, ticks = F,
                width = "50%"
              )
            }
          })
        ),
        footer = 
          actionButton(
            inputId = "go.pred",
            label = "Predict",
            icon = icon("chart"),
            status = "success"
          )
      )
    )
  })
  
  val.pred <- reactive({
    if(is.null(input[["p.Gender"]])) return(1)
    
    df <- 
      tibble(
        "Departure/Arrival time convenient" = input[["p.Departure/Arrival time convenient"]],
        "Gender" = input[["p.Gender"]],
        "Customer Type" = input[["p.Customer Type"]],
        "Age" = input[["p.Age"]],
        "Type of Travel" = input[["p.Type of Travel"]],
        "Class" = input[["p.Class"]],
        "Flight Distance" = input[["p.Flight Distance"]],
        "Inflight wifi service" = input[["p.Inflight wifi service"]],
        "Ease of Online booking" = input[["p.Ease of Online booking"]],
        "Gate location" = input[["p.Gate location"]],
        "Food and drink" = input[["p.Food and drink"]],
        "Online boarding" = input[["p.Online boarding"]],
        "Seat comfort" = input[["p.Seat comfort"]],
        "Inflight entertainment" = input[["p.Inflight entertainment"]],
        "On-board service" = input[["p.On-board service"]],
        "Leg room service" = input[["p.Leg room service"]],
        "Baggage handling" = input[["p.Baggage handling"]],
        "Checkin service" = input[["p.Checkin service"]],
        "Inflight service" = input[["p.Inflight service"]],
        "Cleanliness" = input[["p.Cleanliness"]],
        "Departure Delay in Minutes" = input[["p.Departure Delay in Minutes"]],
        "Arrival Delay in Minutes" = input[["p.Arrival Delay in Minutes"]]
      )
    
    predict(
      model.tree,
      df,
      type = "prob"
    )[2] %>% round(2)
  })
  
  observeEvent(input$go.pred, {
    removeModal()
  })
  
  observeEvent(input$go.importance, {
    showModal(
      modalDialog(
        size = "l",
        title = "Passenger data importance",
        plotOutput("model.importance")
      )
    )
  })
  
  output$model.importance <- renderPlot({
    model.tree$variable.importance  %>% 
      data.frame(
        name = names(.),
        value = .
      ) %>%
      ggplot(
        aes(
          y = reorder(name, value),
          x = value
        )
      ) + geom_col() + theme_minimal() +
      scale_fill_brewer() + labs(x = "Factor Importance", y = "Factor")
  })
  
#### Data Logic ####
  df.f <- reactive({
    df <- df.raw
    
    for(i in 1:22) {
      x = df.names[i]
      if(!(x %in% c("Gender", "Customer Type", "Type of Travel", "Class")) & !is.null(input[[paste0("f.", x)]])) {
        df <- 
          df %>% 
          filter(
            between(
              get(x),
              input[[paste0("f.", x)]][1],
              input[[paste0("f.", x)]][2]
              )
          ) 
      }
    }
    df
  })
  
  df.drill <- reactive({
    brush <- event_data("plotly_selected", source = "go2f")
    
    if(!is.null(brush)) {
        df.f() %>%
        filter(
          `id` %in% brush$customdata
        )     
    } else {
      df.f()
    }
  })
#### Visuals ####
# Value Boxes
  output$vbox.kpi <- renderUI({
    valueBox(
      value = h1(
        (nrow(df.drill()[df.drill()$satisfaction == "satisfied",])/
        nrow(df.drill())) %>% round(., 2)
      ), elevation = 2, width = 12, color = case_when(
        (nrow(df.drill()[df.drill()$satisfaction == "satisfied",])/
           nrow(df.drill())) %>% round(., 2) <  .35 ~ "maroon",
        (nrow(df.drill()[df.drill()$satisfaction == "satisfied",])/
           nrow(df.drill())) %>% round(., 2) <= .43 ~ "warning",
        T ~ "success"
      ),
      subtitle = "%age of satisfied passengers",
      icon = icon(name = "smile-beam", lib = "font-awesome")
    )
  })
  output$vbox.n <- renderUI({
    valueBox(
      value = h1(nrow(df.drill())), elevation = 2, width = 12, color = "olive",
      subtitle = "Number of passengers",
      icon = icon(name = "users", lib = "font-awesome")
    )
  })
  output$vbox.pred <- renderUI({
    valueBox(
      value = h1(paste(if_else(val.pred() > .5, "Satisfied!:", "Disapointed"), val.pred())),
      elevation = 2, width = 12,
      color = if_else(if_else(val.pred() > .5, "satisfied", "Not this time") == "satisfied", "success", "danger"),
      subtitle = "Predicted passenger reaction with given data",
      icon = icon(
        name = if_else(
          if_else(
            val.pred() > .5, "satisfied", "Duck") == "satisfied", "smile", "fa-duck"),
        lib = "font-awesome"
      )
    )
  })
  output$vbox.pred.char <- renderUI({
    valueBox(
      value = h1(similar.pass()), elevation = 2, width = 12, color = "gray",
      subtitle = "Similar passengers",
      icon = icon(name = "poll", lib = "font-awesome")
    )
  })
  output$vbox.pred.cust <- renderUI({
    valueBox(
      value = h1(similar.char()), elevation = 2, width = 12, color = "gray",
      subtitle = "Similar impression",
      icon = icon(name = "star", lib = "font-awesome")
    )
  })
# DT
  output$plot.bars <- renderPlotly({
    df <- 
      df.drill() %>%
      mutate("var1" = get(input$pick1), "var2" = get(input$pick2)) %>%
        group_by(var1, var2, satisfaction) %>%
        summarise(count = n()) %>%
        mutate(percentage = count/sum(count))

    
    df <- df %>%
      ggplot(
        aes(
          y = var1,
          x = count,
          fill = satisfaction
        )
      ) + geom_col()  +
      theme_minimal() +
      scale_fill_brewer() +
      facet_wrap(~var2, ncol = 1) + 
      labs(title = "", subtitle = "", y = "", x = "")
      ggplotly(df) %>% hide_legend() %>%
        layout(
          title = input$pick2,
          xaxis = list(title = "Total count"),
          yaxis = list(title = input$pick1)
        )
  })
  
  # Scatter plot - Fligth Distance vs. Departure Delay
  output$plot.flightscatter <- renderPlotly({

    df.f() %>%
      plot_ly(
        x = ~`Flight Distance`,
        y = ~get(input$pick3),
        color = ~satisfaction,
        source = "go2f",
        customdata = ~id
      ) %>%
      add_markers(
        marker = list(opacity = .4)
      ) %>% 
      layout(
        xaxis = list(
          title = "Flight Distance (km)"
        ),
        yaxis = list(
          title = "Delay (min)"
        ), title = "Flight distance & delay\nimpact on passengers' satisfaction",
        legend  = list(
          x = .1, y = .9,
          font = list(
            size = 14,
            color = "#000"),
          bgcolor = "#f7f7f7",
          bordercolor = "#FFFFFF",
          borderwidth = 2
        )
      ) %>% toWebGL()
  })
  
  output$plot.all <- renderEcharts4r({
    df.drill()[,7:20] %>% 
      pivot_longer(cols = everything(), names_to = "Type", values_to = "Value") %>%
      group_by(Type, Value) %>% count() %>% mutate(Value = paste(Value, "-", n)) %>% group_by(Type) %>%
      e_charts(Value, timeline = T) |> 
      e_pie(n, roseType = "radius") %>% e_tooltip() %>%
      e_timeline_serie(
        title = list(
          list(text = "Baggage"),
          list(text = "Checkin service"),
          list(text = "Cleanliness"),
          list(text = "Departure arival"),
          list(text = "Ease of online borading"),
          list(text = "Food & drink"),
          list(text = "Gate location"),
          list(text = "Inflight entertainment"),
          list(text = "Inflight service"),
          list(text = "Inflight wifi service"),
          list(text = "Leg room service"),
          list(text = "On-board service"),
          list(text = "Online boarding"),
          list(text = "Seat comfort")
        )
      )
  })
  
  similar.pass <- reactive({
    if(is.null(input[["p.Inflight wifi service"]])) return(0)
    
    df.raw %>%
      filter(
        `Gender` %in% input[["p.Gender"]],
        `Customer Type` %in% input[["p.Customer Type"]],
         between(`Age`, (input[["p.Age"]]-5), (input[["p.Age"]]+5)),
        `Type of Travel` %in% input[["p.Type of Travel"]],
        `Class` %in% input[["p.Class"]]
      ) %>% nrow
  })
  similar.char <- reactive({
    if(is.null(input[["p.Inflight wifi service"]])) return(0)

    df.raw %>%
      filter(
        between(
          `Inflight wifi service`,
          input[["p.Inflight wifi service"]] - 1,
          input[["p.Inflight wifi service"]] + 1
        ),
        between(
          `Food and drink`,
          input[["p.Food and drink"]] - 1,
          input[["p.Food and drink"]] + 1
        ),
        between(
          `Gate location`,
          input[["p.Ease of Online booking"]] - 1,
          input[["p.Ease of Online booking"]] + 1
        ),
        between(
          `Ease of Online booking`,
          input[["p.Ease of Online booking"]] - 1,
          input[["p.Ease of Online booking"]] + 1
        ),
        between(
          `Online boarding`,
          input[["p.Online boarding"]] - 1,
          input[["p.Online boarding"]] + 1
        ),
        between(
          `Seat comfort`,
          input[["p.Seat comfort"]] - 1,
          input[["p.Seat comfort"]] + 1
        ),
        between(
          `Inflight entertainment`,
          input[["p.Inflight entertainment"]] - 1,
          input[["p.Inflight entertainment"]] + 1
        ),
        between(
          `On-board service`,
          input[["p.On-board service"]] - 1,
          input[["p.On-board service"]] + 1
        ),
        between(
          `Baggage handling`,
          input[["p.Baggage handling"]] - 1,
          input[["p.Baggage handling"]] + 1
        ),
        between(
          `Cleanliness`,
          input[["p.Cleanliness"]] - 1,
          input[["p.Cleanliness"]] + 1
        ),
        between(
          `Departure/Arrival time convenient`,
          input[["p.Departure/Arrival time convenient"]] - 1,
          input[["p.Departure/Arrival time convenient"]] + 1
        ),
        between(
          `Gate location`,
          input[["p.Gate location"]] - 1,
          input[["p.Gate location"]] + 1
        ),
        between(
          `Food and drink`,
          input[["p.Food and drink"]] - 1,
          input[["p.Food and drink"]] + 1
        ),
        between(
          `Inflight service`,
          input[["p.Inflight service"]] - 1,
          input[["p.Inflight service"]] + 1
        ),
        between(
          `Leg room service`,
          input[["p.Leg room service"]] - 1,
          input[["p.Leg room service"]] + 1
        )
      ) %>% nrow
  })
}

#### Run App ####
shinyApp(
  ui = bs4Dash::dashboardPage(
    header, sidebar, body,
    dark = NULL, fullscreen = T, scrollToTop = T, freshTheme = T
  ),
  server
)