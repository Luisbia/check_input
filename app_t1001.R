library(rio)
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(rlang)
library(DT)
library(data.table)
library(scales)
library(dataregacc)
library(regacc)
source("utils/palette.R")
source("utils/theme_shiny.R")



# Data manipulation----
## import files
df <-list.files(path="data/csv",
                pattern= glob2rx(paste0("*",country_sel,"*")),
                full.names=TRUE) %>% 
  as_tibble() %>% 
  mutate(date=map(value,file.mtime)) %>% 
  unnest(date) %>% 
  arrange(desc(date)) %>% 
  head(1) %>% 
  select(value) %>% 
  pull() %>% 
  fread() %>% 
  mutate(NUTS=as.factor(NUTS)) %>% 
  filter(table_identifier=="T1001") %>% 
  as.data.table()

#bring labels
NUTS2021<-NUTS_2021 %>% 
  filter(country %in% country_sel) %>% 
  mutate(across(c(NUTS,label),as_factor)) %>% 
  mutate(country=as.character(country)) %>% 
  as.data.table()


# rename volume GVA

temp <- copy(df) %>% 
.[unit_measure== "PC",.(type, ref_area, NUTS, sto,time_period, obs_value)] %>% 
.[,sto := str_replace(sto, "B1G", "B1Gv")]

df<- df[unit_measure!="PC"] %>% 
.[,.(type, ref_area, NUTS, sto,time_period, obs_value)]


df<- rbindlist(list(df,temp))

df <-  left_join(df,NUTS2021) %>% 
  .[NUTS !="1",]


# B1G per PS/POP
B1G <- as.data.table(df) %>%
.[sto %in% c("B1G", "EMP", "POP")] %>% 
dcast(...~sto, value.var = "obs_value") %>% 
.[,B1G_EMP := round(B1G * 1000 / EMP, digits = 1)] %>% 
.[, B1G_POP := round(B1G * 1000 / POP, digits = 1)] %>%
.[,!c("B1G", "EMP", "POP"),with=FALSE] %>% 
 melt(measure.vars = c("B1G_EMP", "B1G_POP"),
            variable.name = "sto",
            value.name = "obs_value")

EMP_POP <- as.data.table(df) %>% 
.[sto %in% c("POP", "EMP")] %>%
dcast(...~sto, value.var = "obs_value") %>% 
.[,EMP_POP := round(EMP * 100 / POP, digits = 0)] %>% 
.[,!c("EMP", "POP"),with=FALSE] %>% 
melt(measure.vars = c("EMP_POP"),
       variable.name = "sto",
       value.name = "obs_value")

df <- rbindlist(list(df, B1G, EMP_POP), use.names = TRUE)

# share in country

df[, share_nat := round(obs_value * 100 / obs_value[NUTS == "0"],1), by = list(type, sto, time_period)]

# time mean
df[, time_mean := round(obs_value * 100 / mean(obs_value, na.rm = TRUE),1), by = list(ref_area, type, sto)]

df <- df %>%
  na.omit() %>% 
  melt(measure.vars = c("obs_value","share_nat","time_mean"),
       variable.name = "unit_measure",
       value.name = "obs_value") %>% 
  dcast(...~type, value.var = "obs_value") %>% 
  .[, rev:= round(T - V,1)] %>% 
  .[, revp := round(rev * 100 / V,1)] %>% 
  melt(measure.vars = c("T","V","rev","revp"),
       variable.name = "type",
       value.name = "obs_value") 

# growth rates
temp <- as.data.table(df) %>%
  .[type %in% c("T","V") & unit_measure == "obs_value",] %>%
  .[order(time_period),growth := round((obs_value - lag(obs_value)) * 100 / lag(obs_value), digits = 1),
    by = c("type", "ref_area", "sto")] %>% 
  .[,!c("obs_value", "unit_measure"),with=FALSE] %>% 
  setnames("growth","obs_value") %>% 
  .[,unit_measure:="growth"]

df <- rbindlist(list(df, temp), use.names = TRUE) %>% 
  .[NUTS !="1",]

### Data for shiny----


df_dotplot <- as.data.table(df) %>%
  dcast(...~type, value.var = "obs_value") 

df_scatter <- df_dotplot

df_data <- df 


# Shiny ui----
ui <- fluidPage(theme = shinytheme("flatly"),
                HTML('<meta name="viewport" content="width=1280">'),
                
                # Setting the relative size of the sidebar and the plot area:
                tags$head(
                  tags$style(HTML(".col-lg-4 { width: 20%;}
                           .col-lg-8 { width: 80%;}")),
                  # Title to be shown @ browser (to get rid of html tags)
                  tags$title("Regional Accounts Data Analysis")
                ),
  tabsetPanel(
    tabPanel("Line plot",
      sidebarLayout(
        sidebarPanel(
          sliderInput("time_period", "Time range",
                      min = min(df$time_period), max = max(df$time_period),
                      value = c(min(df$time_period), max = max(df$time_period)),
                      step = 1,
                      round = TRUE
          ),
          checkboxInput(inputId = "freey", "Free Y axis", value = TRUE),
          checkboxInput(inputId = "leg", "Show Legend", value = FALSE),
                    selectInput("group", "Group by:",
                      choices = c("ref_area", "sto", "type"),
                      selected = "ref_area",
                      multiple = FALSE
          ),
          selectInput("colour", "Colour by:",
            choices = c("ref_area", "sto", "type", "NUTS"),
            selected = "NUTS",
            multiple = FALSE
          ),
           selectInput("sto", "NA item:",
             choices = unique(df$sto),
             selected = "B1G",
             multiple = TRUE
           ),
          selectInput("facet", "Small multiples:",
            choices = c("ref_area", "sto", "type", "unit_measure"),
            selected = "sto",
            multiple = FALSE
          ),
          selectInput("type", "Type of series:",
            choices = unique(df$type),
            selected = "T",
            multiple = TRUE
          ),
          selectInput("unit_measure", "Unit of the series:",
            choices = unique(df$unit_measure),
            selected = "growth",
            multiple = TRUE
          ),
          selectInput("ref_area", "Regions:",
            choices = unique(df$ref_area),
            selected = unique(df$ref_area),
            multiple = TRUE)
          ),

        mainPanel(plotlyOutput("linePlot", width = "100%", height = "800px")),
          
          )#mainPanel
        ), #tabPanel
    
    tabPanel("Dot plot",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("time_period1", "Time range",
                             min = min(df_dotplot$time_period), max = max(df_dotplot$time_period),
                             value = c(min(df_dotplot$time_period), max = max(df_dotplot$time_period)),
                             step = 1,
                             round = TRUE
                 ),
                 checkboxInput(inputId = "freex", "Free X axis", value = TRUE),
                 checkboxInput(inputId = "leg1", "Show Legend", value = FALSE),
                 selectInput("yaxis1", "Y axis:",
                             choices = c( "ref_area", "sto"),
                             selected = "ref_area"
                 ),
                 selectInput("xaxis1", "X axis:",
                             choices = c("T", "V", "rev", "revp"),
                             selected = "T"
                 ),
                 selectInput("colour1", "Colour by:",
                             choices = c("ref_area", "NUTS", "sto", "unit_measure","type"),
                             selected = "ref_area"
                 ),
                 selectInput("sto1", "NA item:",
                             choices = unique(df_dotplot$sto),
                             selected = "B1G_POP",
                             multiple = TRUE
                 ),
                 selectInput("facet1", "Small multiples:",
                             choices = c("ref_area", "sto", "time_period", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 
                 selectInput("unit_measure1", "Unit of the series:",
                             choices = unique(df_dotplot$unit_measure),
                             selected = "obs_value",
                             multiple = TRUE
                 ),
                 selectInput("ref_area1", "Regions:",
                             choices = unique(df_dotplot$ref_area),
                             selected = unique(df_dotplot$ref_area),
                             multiple = TRUE
                 )
               ),
               
               mainPanel(plotlyOutput("dotPlot", width = "100%", height = "800px")),
               
             )#mainPanel
    ),    #tabPanel
    
    tabPanel("Scatter plot",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("time_period2", "Time range",
                             min = min(df_scatter$time_period), max = max(df_dotplot$time_period),
                             value = c(min(df_scatter$time_period), max = max(df_dotplot$time_period)),
                             step = 1,
                             round = TRUE
                 ),
                 checkboxInput(inputId = "freexy", "Free X and Y axis", value = TRUE),
                 checkboxInput(inputId = "leg2", "Show Legend", value = FALSE),
                 selectInput("xaxis2", "X axis:",
                             choices = c("T", "V", "rev", "revp"),
                             selected = "rev"
                 ),
                 selectInput("yaxis2", "Y axis:",
                             choices = c("T", "V", "rev", "revp"),
                             selected = "revp"
                 ),
                 selectInput("colour2", "Colour by:",
                             choices = c("ref_area", "NUTS", "sto", "unit_measure"),
                             selected = "ref_area"
                 ),
                 selectInput("sto2", "NA item:",
                             choices = unique(df_scatter$sto),
                             selected = "B1G",
                             multiple = TRUE
                 ),
                 selectInput("facet2", "Small multiples:",
                             choices = c("¡ref_area", "sto", "time_period", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 
                 selectInput("unit_measure2", "Unit of the series:",
                             choices = unique(df_scatter$unit_measure),
                             selected = "obs_value",
                             multiple = TRUE
                 ),
                 selectInput("ref_area2", "Regions:",
                             choices = unique(df_scatter$ref_area),
                             selected = unique(df_scatter$ref_area),
                             multiple = TRUE
                 )
               ),
               
               mainPanel(plotlyOutput("scatterPlot", width = "100%", height = "800px")),
               
             )#mainPanel
    ),
    # Data----
    tabPanel(
      "Data",
      sidebarLayout(
        sidebarPanel(
          selectInput("type5", "Type:",
                      choices = unique(df_data$type),
                      selected = "T",
                      multiple = TRUE
          ),
          selectInput("sto5", "NA Item:",
                      choices = unique(df_data$sto),
                      selected = unique(df_data$sto),
                      multiple = TRUE
          ),
          selectInput("unit_measure5", "Unit:",
                      choices = unique(df_data$unit_measure),
                      selected = "obs_value",
                      multiple = TRUE
          ),
          selectInput("ref_area5", "Regions:",
                      choices = unique(df_data$ref_area),
                      selected = unique(df_data$ref_area),
                      multiple = TRUE)
        ),
        
        mainPanel(DTOutput("data")
        )))  
    ) #tabsetpanel
)#fluid page


# Shiny server----

server <- function(input, output) {
  df_filter <- reactive({df %>%
                          filter(ref_area %in% input$ref_area &
                                   time_period >= input$time_period[1] &
                                   time_period <= input$time_period[2] &
                                   sto %in% input$sto &
                                   type %in% input$type &
                                   unit_measure %in% input$unit_measure)})
  
  output$linePlot <- renderPlotly({

    # Build  plot
    p <- ggplot(df_filter(), aes(time_period, obs_value, group=!!parse_expr(input$group),colour = !!parse_expr(input$colour),  label = label)) +
      geom_line(size = 0.8)+
      theme_shiny_line+
      scale_colour_luis()+
      scale_y_continuous(breaks = pretty_breaks(3), labels = label_number(),expand=c(0,0.2))+
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0,0.2))
    
    # legend
    
    if (input$leg == FALSE) {
      p <- p + theme(legend.position = "none")
    }    else    {
      p <- p + theme(legend.position = "right")
    }
    # y axis from checkbox
    if (input$freey == FALSE) {
      p <- p + facet_wrap(~ get(input$facet))
    }    else  {
      p <- p + facet_wrap(~ get(input$facet), scales = "free_y")
    }
    

    # pass the plot to ggplotly and choose dimensions
    ggplotly(p)%>% config(displayModeBar = F)#, width = 800, height = 800)
  } )

  df_filter1 <- reactive(df_dotplot %>%
                           filter(ref_area %in% input$ref_area1 &
                                    time_period >= input$time_period1[1] &
                                    time_period <= input$time_period1[2] &
                                    sto %in% input$sto1 &
                                    unit_measure %in% input$unit_measure1))
  
  output$dotPlot <- renderPlotly({
    
    # filter data set with user input
    
    # Build  plot
    p1 <- ggplot(df_filter1(), aes(x = !!parse_expr(input$xaxis1), fct_reorder(!!parse_expr(input$yaxis1), !!parse_expr(input$xaxis1)), colour = !!parse_expr(input$colour1), label = label)) +
      geom_point(size= 2) +
      theme_shiny_scatter +
      scale_colour_luis() +
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1))
     
    # legend
    
    if (input$leg1 == FALSE) {
      p1 <- p1 + theme(legend.position = "none")
    }    else    {
      p1 <- p1 + theme(legend.position = "bottom")
    }
    
    
    # y axis from checkbox
    if (input$freex == FALSE) {
      p1 <- p1 + facet_wrap(~ get(input$facet1))
    }    else  {
      p1 <- p1 + facet_wrap(~ get(input$facet1), scales = "free")
    }
    
    # pass the plot to ggplotly and choose dimensions
    ggplotly(p1)%>% config(displayModeBar = F)#, width = 800, height = 800)
  } )
  
  df_filter2 <- reactive({df_scatter %>%
      filter(ref_area %in% input$ref_area2 &
               time_period >= input$time_period2[1] &
               time_period <= input$time_period2[2] &
               sto %in% input$sto2 &
               unit_measure %in% input$unit_measure2)})
  
  output$scatterPlot <- renderPlotly({
    # filter data set with user input
    
    
    # Build  plot
    p2 <- ggplot(df_filter2(), aes(!!parse_expr(input$xaxis2), !!parse_expr(input$yaxis2), colour = !!parse_expr(input$colour2), label = label)) +
      geom_point(size= 2) +
      theme_shiny_scatter +
      scale_colour_luis() +
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0,0.2))+
      scale_y_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0,0.2))
    
    
    # legend
    
    if (input$leg2 == FALSE) {
      p2 <- p2 + theme(legend.position = "none")
    }    else    {
      p2 <- p2 + theme(legend.position = "bottom")
    }
    
    
    # y axis from checkbox
    if (input$freexy == FALSE) {
      p2 <- p2 + facet_wrap(~ get(input$facet2))
    }  else   {
      p2 <- p2 + facet_wrap(~ get(input$facet2), scales = "free")
    }
    
    # pass the plot to ggplotly and choose dimensions
    ggplotly(p2)%>% config(displayModeBar = F)#, width = 800, height = 800)
  } )
  
  df_filter5 <- reactive({df_data %>%
      filter(ref_area %in% input$ref_area5 &
               type %in% input$type5 &
               unit_measure %in% input$unit_measure5 &
               sto %in% input$sto5)})
  
  output$data <- renderDT({
    datatable(df_filter5(),
              filter = "top",
              class = "stripe hover",
              extensions = c("Buttons"), rownames = FALSE,
              options = list(
                lengthMenu = list(c(50, 200, 500), c("50", "200", "500")),
                pageLength = 50,
                autoFill = TRUE,
                dom = "Blfrtip",
                buttons = c("excel")
              )
    )
  })
  }

# Run the application

shinyApp(ui = ui, server = server)
