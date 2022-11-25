library(tidyverse)
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(rlang)
library(DT)
library(data.table)
library(scales)
library(arrow)
library(bslib)
library(dataregacc)
source("utils/palette.R")
source("utils/theme_report.R")
#library(reactlog)

#reactlog_enable()

df_new <- read_parquet("data/new.parquet") %>% 
  select(-obs_status,-value,-date) %>% 
  filter(country %in% country_sel & table %in% table_sel) %>% 
  rename(new=obs_value)

df_prev <- dataregacc::eurobase %>% 
  select(-obs_status) %>% 
  filter(country %in% country_sel & table %in% table_sel) %>% 
  rename(prev=obs_value) 


df <- full_join(df_prev,df_new) %>% 
  relocate(prev,.before=new) %>% 
  mutate(rev=round(new-prev),
         revp=round(rev*100/prev,1)) %>% 
  mutate(NUTS=as.factor(NUTS)) %>% 
  left_join(.,NUTS_2021) %>% 
  filter(label != "Extra-regio") %>%
  mutate(label = paste0(ref_area, "-", label)) %>% 
  pivot_longer(cols=c(new,prev,rev,revp),
               names_to="type",
               values_to="obs_value")


themes <- c("cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "litera", "lumen",  "minty", "pulse", "sandstone", "simplex", "spacelab", "superhero", "united", "yeti")

random_theme <- sample(themes, 1)


# Shiny ui----
ui <- fluidPage( # theme = shinytheme("flatly"),
  theme = bs_theme(bootswatch = random_theme), # choose https://bootswatch.com/
  HTML('<meta name="viewport" content="width=1280">'),

  # Setting the relative size of the sidebar and the plot area:
  tags$head(
    tags$style(HTML(".col-lg-4 { width: 20%;}
                           .col-lg-8 { width: 80%;}")),
    # Title to be shown @ browser (to get rid of html tags)
    tags$title("Regional Accounts Data Analysis")
  ),
  tabsetPanel(
    tabPanel(
      "LinePlot",
      sidebarLayout(
        sidebarPanel(
          textOutput("text"),
          sliderInput("time_period", "Time range",
            min = min(df$time_period), max = max(df$time_period),
            value = c(min(df$time_period), max = max(df$time_period)),
            step = 1,
            round = TRUE
          ),
          checkboxInput(inputId = "freey", "Free Y axis", value = FALSE),
          checkboxInput(inputId = "leg", "Show Legend", value = FALSE),
          selectInput("group", "Group by:",
            choices = c("type", "table", "country", "ref_area", "activity", "sto", "unit_measure", "NUTS", "accounting_entry","label"),
            selected = "label",
            multiple = FALSE
          ),
          selectInput("colour", "Colour by:",
            choices = c("type", "table", "country", "ref_area", "activity", "sto", "unit_measure", "NUTS", "accounting_entry","label"),
            selected = "type",
            multiple = FALSE
          ),
          selectInput("facet", "Small multiples:",
            choices = c("type", "table", "country", "ref_area", "activity", "sto", "unit_measure", "NUTS", "accounting_entry","label"),
            selected = "table",
            multiple = FALSE
          ),
          selectInput("type", "Type:",
            choices = unique(df$type),
            selected = "new",
            multiple = TRUE
          ),
          selectInput("sto", "NA item:",
            choices = unique(df$sto),
            selected = df$sto[1],
            multiple = TRUE
          ),
          selectInput("table", "Table:",
            choices = unique(df$table),
            selected = df$table[1],
            multiple = TRUE
          ),
          selectInput("ref_area", "Region",
            choices = unique(df$ref_area),
            selected = unique(df$ref_area),
            multiple = TRUE
          ),
          selectInput("country", "Country:",
            choices = unique(df$country),
            selected = df$country[1],
            multiple = TRUE
          ),
          selectInput("accounting_entry", "Accounting Entry:",
            choices = unique(df$accounting_entry),
            selected = df$accounting_entry[1],
            multiple = TRUE
          ),
          selectInput("unit_measure", "Unit:",
            choices = unique(df$unit_measure),
            selected = df$unit_measure[1],
            multiple = TRUE
          ),
          selectInput("NUTS", "NUTS level:",
            choices = unique(df$NUTS),
            selected = "2",
            multiple = TRUE
          ),
          selectInput("activity", "NACE:",
            choices = unique(df$activity),
            selected = df$activity[1],
            multiple = TRUE
          )
        ),
        mainPanel(plotlyOutput("lineplot", width = "100%", height = "800px")),
      ) # mainPanel
    ),
    tabPanel(
      "DotPlot",
      sidebarLayout(
        sidebarPanel(
          sliderInput("time_period1", "Time range",
            min = min(df$time_period), max = max(df$time_period),
            value = c(min(df$time_period), max = max(df$time_period)),
            step = 1,
            round = TRUE
          ),
          checkboxInput(inputId = "freex", "Free X axis", value = FALSE),
          checkboxInput(inputId = "leg1", "Show Legend", value = FALSE),
          selectInput("group1", "Group by:",
            choices = c("type", "table", "country","ref_area", "activity", "sto", "unit_measure", "NUTS", "accounting_entry","label" ),
            selected = "label",
            multiple = FALSE
          ),
          selectInput("colour1", "Colour by:",
            choices = c("type", "table", "country", "ref_area", "activity", "sto", "unit_measure", "NUTS", "accounting_entry","label"),
            selected = "type",
            multiple = FALSE
          ),
          selectInput("facet1", "Small multiples:",
            choices = c("type", "table", "country", "ref_area", "activity", "sto", "unit_measure", "NUTS", "accounting_entry","label"),
            selected = "table",
            multiple = FALSE
          ),
          selectInput("type1", "Type:",
            choices = unique(df$type),
            selected = "new",
            multiple = TRUE
          ),
          selectInput("sto1", "NA item:",
            choices = unique(df$sto),
            selected = df$sto[1],
            multiple = TRUE
          ),
          selectInput("table1", "Table:",
            choices = unique(df$table),
            selected = df$table[1],
            multiple = TRUE
          ),
          selectInput("ref_area1", "Region",
            choices = unique(df$ref_area),
            selected = unique(df$ref_area),
            multiple = TRUE
          ),
          selectInput("country1", "Country:",
            choices = unique(df$country),
            selected = df$country[1],
            multiple = TRUE
          ),
          selectInput("accounting_entry1", "Accounting Entry:",
            choices = unique(df$accounting_entry),
            selected = df$accounting_entry[1],
            multiple = TRUE
          ),
          selectInput("unit_measure1", "Unit:",
            choices = unique(df$unit_measure),
            selected = df$unit_measure[1],
            multiple = TRUE
          ),
          selectInput("NUTS1", "NUTS level:",
            choices = unique(df$NUTS),
            selected = "2",
            multiple = TRUE
          ),
          selectInput("activity1", "NACE:",
            choices = unique(df$activity),
            selected = df$activity[1],
            multiple = TRUE
          )
        ),
        mainPanel(plotlyOutput("dotplot", width = "100%", height = "800px")),
      ) # mainPanel
    ),
    tabPanel(
      "Data",
      sidebarLayout(
        sidebarPanel(
          sliderInput("time_period2", "Time range",
            min = min(df$time_period), max = max(df$time_period),
            value = c(min(df$time_period), max = max(df$time_period)),
            step = 1,
            round = TRUE
          ),
          selectInput("table2", "Table:",
            choices = unique(df$table),
            selected = df$table[1],
            multiple = TRUE
          ),
          selectInput("NUTS2", "NUTS:",
            choices = unique(df$NUTS),
            selected = "2",
            multiple = TRUE
          ),
          selectInput("country2", "Country:",
            choices = unique(df$country),
            selected = df$country[1],
            multiple = TRUE
          ),
          selectInput("ref_area2", "Region",
            choices = unique(df$ref_area),
            selected = unique(df$ref_area),
            multiple = TRUE
          ),
          selectInput("accounting_entry2", "Accounting Entry:",
            choices = unique(df$accounting_entry),
            selected = df$accounting_entry[1],
            multiple = TRUE
          ),
          selectInput("sto2", "Variable:",
            choices = unique(df$sto),
            selected = df$sto[1],
            multiple = TRUE
          ),
          selectInput("activity2", "Activity:",
            choices = unique(df$activity),
            selected = df$activity[1],
            multiple = TRUE
          ),
          selectInput("type2", "Type:",
            choices = unique(df$type),
            selected = "new",
            multiple = TRUE
          ),
          selectInput("unit_measure2", "Unit:",
            choices = unique(df$unit_measure),
            selected = df$unit_measure[1],
            multiple = TRUE
          ),
        ),
        mainPanel(DTOutput("data")),
      )
    )
  ) # tabsetpanel
) # fluid page

# Shiny server----

server <- function(input, output, session) {
  output$text <- renderText({
    random_theme
  })
  
  regions <- reactive({
    filter(df, country %in% input$country & NUTS %in% input$NUTS)
  })
  
  observeEvent(regions(), {
    choices <- unique(regions()$ref_area)
    updateSelectInput(inputId = "ref_area", choices = choices,selected=choices) 
  })
  
  na_item <- reactive({
    filter(df, table %in% input$table)
  })
  
  observeEvent(na_item(), {
    choices <- unique(na_item()$sto)
    updateSelectInput(inputId = "sto", choices = choices,selected=choices) 
  })
 
  df_filter2 <- reactive({
    df %>%
      filter(table %in% input$table &
             NUTS %in% input$NUTS &
              ref_area %in% input$ref_area &
        type %in% input$type &
        country %in% input$country &
        accounting_entry %in% input$accounting_entry &
        sto %in% input$sto &
        activity %in% input$activity &
        unit_measure %in% input$unit_measure &
        time_period >= input$time_period[1] &
        time_period <= input$time_period[2])
  })

  output$lineplot <- renderPlotly({


    # Build  plot
    p <- ggplot(df_filter2(), aes(time_period, obs_value, group = !!parse_expr(input$group), colour = !!parse_expr(input$colour), label = label)) +
      geom_line(size = 0.7) +
      theme_regacc_line +
      scale_colour_luis() +
      scale_y_continuous(breaks = pretty_breaks(3), labels = label_number(),expand=c(0,0.2))+
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(1),expand=c(0,0.2))


    # legend

    if (input$leg == FALSE) {
      p <- p + theme(legend.position = "none")
    } else {
      p <- p + theme(legend.position = "bottom")
    }
    # y axis from checkbox
    if (input$freey == FALSE) {
      p <- p + facet_wrap(~ get(input$facet))
    } else {
      p <- p + facet_wrap(~ get(input$facet), scales = "free_y")
    }

    # pass the plot to ggplotly and choose dimensions
    ggplotly(p) %>% config(displayModeBar = F) # , width = 800, height = 800)
  })

  regions1 <- reactive({
    filter(df, country %in% input$country1 & NUTS %in% input$NUTS1)
  })
  
  observeEvent(regions1(), {
    choices <- unique(regions1()$ref_area)
    updateSelectInput(inputId = "ref_area1", choices = choices,selected=choices) 
  })
  
  na_item1 <- reactive({
    filter(df, table %in% input$table1)
  })
  
  observeEvent(na_item1(), {
    choices <- unique(na_item1()$sto)
    updateSelectInput(inputId = "sto1", choices = choices,selected=choices) 
  })
 
  df_filterb2 <- reactive({
    df %>%
      filter(table %in% input$table1 &
               NUTS %in% input$NUTS1 &
               ref_area %in% input$ref_area1 &
        type %in% input$type1 &
        accounting_entry %in% input$accounting_entry1 &
        sto %in% input$sto1 &
          country %in% input$country1 &
        activity %in% input$activity1 &
        unit_measure %in% input$unit_measure1 &
        time_period >= input$time_period1[1] &
        time_period <= input$time_period1[2])
  })

  output$dotplot <- renderPlotly({
    # Build  plot
    p1 <- ggplot(df_filterb2(), aes(obs_value, reorder(ref_area, obs_value), group = !!parse_expr(input$group1), colour = !!parse_expr(input$colour1), label = time_period)) +
      geom_point(size = 2) +
      theme_regacc_scatter +
      scale_colour_luis()+
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1))


    # legend

    if (input$leg1 == FALSE) {
      p1 <- p1 + theme(legend.position = "none")
    } else {
      p1 <- p1 + theme(legend.position = "bottom")
    }
    # y axis from checkbox
    if (input$freex == FALSE) {
      p1 <- p1 + facet_wrap(~ get(input$facet1))
    } else {
      p1 <- p1 + facet_wrap(~ get(input$facet1), scales = "free")
    }

    # pass the plot to ggplotly and choose dimensions
    ggplotly(p1) %>% config(displayModeBar = F)
  })

  regions2 <- reactive({
    filter(df, country %in% input$country2 & NUTS %in% input$NUTS2)
  })
  
  observeEvent(regions2(), {
    choices <- unique(regions2()$ref_area)
    updateSelectInput(inputId = "ref_area2", choices = choices,selected=choices) 
  })
  
  na_item2 <- reactive({
    filter(df, table %in% input$table2)
  })
  
  observeEvent(na_item2(), {
    choices <- unique(na_item2()$sto)
    updateSelectInput(inputId = "sto2", choices = choices,selected=choices) 
  })
 
  df_filterc2 <- reactive({
    df %>%
      filter(table %in% input$table2 &
               NUTS %in% input$NUTS2 &
               ref_area %in% input$ref_area2 &
        type %in% input$type2 &
        accounting_entry %in% input$accounting_entry2 &
        sto %in% input$sto2 &
          country %in% input$country2 &
        activity %in% input$activity2 &
        unit_measure %in% input$unit_measure2 &
        time_period >= input$time_period2[1] &
        time_period <= input$time_period2[2])
  })


  output$data <- renderDT(
    datatable(df_filterc2(),
      filter = "top",
      class = "stripe hover",
      extensions = c("Buttons"), rownames = FALSE,
      options = list(
        lengthMenu = list(c(50, 200, -1), c("50", "200", "All")),
        pageLength = 50,
        autoFill = TRUE,
        dom = "Blfrtip",
        buttons = c("csv", "excel")
      )
    )
  )
}

# Run the application

shinyApp(ui = ui, server = server)