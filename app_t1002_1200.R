# Luis Biedma
# Unit C2
# Eurostat
# 2020


 library(rio)
 library(shiny)
 library(shinythemes)
 library(tidyverse)
 library(plotly)
 library(rlang)
 library(DT)
 library(data.table)
 library(scales)
 source("utils/palette.R")
 source("utils/theme_report.R")

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
   filter(table_identifier %in% c("T1002","T1200"))

 #bring labels
 NUTS2021<-NUTS_2021 %>% 
   filter(country %in% country_sel) %>% 
   mutate(across(c(NUTS,label),as_factor)) %>% 
   mutate(country=as.character(country)) %>% 
   as.data.table()


  # rename hours worked

t1002 <- copy(df) %>%
  .[table_identifier == "T1002",.(ref_area, type, sto, activity, time_period, obs_value)] %>% 
  dcast(...~sto, value.var = "obs_value") %>% 
  .[,ESE:= EMP - SAL ] %>% 
  melt(measure.vars = c("EMP", "D1", "P51G", "SAL", "ESE"),
       variable.name = "sto",
       value.name = "obs_value") %>% 
  .[,sto:= str_replace(sto, "EMP", "EMPhw")] %>% 
  .[,sto:= str_replace(sto, "SAL", "SALhw")] %>% 
  .[,sto:= str_replace(sto, "ESE", "ESEhw")] 


t1200 <- copy(df) %>% 
  .[table_identifier == "T1200",.(ref_area, type, sto, activity, time_period, obs_value)] %>% 
  dcast(...~sto, value.var = "obs_value") %>% 
  .[,ESE:= EMP - SAL ] %>% 
  melt(measure.vars = c("B1G", "EMP", "POP", "SAL", "ESE"),
       variable.name = "sto",
       value.name = "obs_value")


df <- rbindlist (list(t1002, t1200)) %>% 
  .[,activity:=str_replace(activity,"_T","TOTAL")]

df <- left_join (df,NUTS2021) %>% 
  .[NUTS !="1"]

# D1 per HW and PS
D1 <- copy(df) %>%
  .[sto %in% c("D1", "SALhw", "SAL") & NUTS != "3" & activity != "_Z",] %>% 
  dcast(...~sto, value.var = "obs_value") %>% 
  .[,D1_SALhw := round(D1 * 1000 / SALhw, digits = 1)] %>% 
  .[,D1_SAL := round(D1 * 1000 / SAL, digits = 1)] %>% 
  .[,!c("D1", "SALhw", "SAL"),with=FALSE] %>% 
  melt(measure.vars = c("D1_SAL", "D1_SALhw"),
       variable.name = "sto",
       value.name = "obs_value") %>% 
  na.omit()

# B1G per HW and PS
B1G <- copy(df) %>%
  .[sto %in% c("B1G", "EMP", "EMPhw") & activity != "_Z",] %>% 
  dcast(...~sto, value.var = "obs_value") %>% 
  .[,B1G_EMP := round(B1G * 1000 / EMP, digits = 1)] %>% 
  .[,B1G_EMPhw := round(B1G * 1000 / EMPhw, digits = 1)] %>% 
  .[,!c("B1G", "EMPhw", "EMP"),with=FALSE] %>% 
  melt(measure.vars = c("B1G_EMP", "B1G_EMPhw"),
       variable.name = "sto",
       value.name = "obs_value") %>% 
  na.omit()

# HW per person

HW_PS <- copy(df) %>%
  .[sto %in% c("EMP", "EMPhw", "SAL", "SALhw", "ESE", "ESEhw") & 
    activity != "_Z" & 
    NUTS != "3",] %>% 
  dcast(...~sto, value.var = "obs_value") %>% 
  .[,HW_EMP := round(EMPhw / EMP, digits = 0)] %>% 
  .[,HW_SAL := round(SALhw / SAL, digits = 0)] %>% 
  .[,HW_ESE := round(ESEhw / ESE, digits = 0)] %>% 
  .[,!c("EMP", "EMPhw", "SAL", "ESE", "SALhw","ESEhw"),with=FALSE] %>% 
  melt(measure.vars = c("HW_EMP", "HW_SAL", "HW_ESE"),
       variable.name = "sto",
       value.name = "obs_value") %>% 
  na.omit()


# Share in GVA of D1 /P51G
B1G_per <- copy(df) %>%
  .[sto %in% c("D1", "P51G", "B1G") & activity != "_Z" & NUTS != "3"] %>% 
  dcast(...~sto, value.var = "obs_value") %>% 
  .[,D1_B1G := round(D1 * 100 / B1G, digits = 2)] %>% 
  .[,P51G_B1G := round(P51G * 100 / B1G, digits = 2)] %>% 
  .[,!c("P51G", "D1", "B1G"),with=FALSE] %>% 
  melt(measure.vars = c("D1_B1G", "P51G_B1G"),
       variable.name = "sto",
       value.name = "obs_value") %>% 
  na.omit()


# ratio EMP/POP
EMP_POP <- copy(df) %>%
  .[sto %in% c("POP", "EMP") & activity %in% c("_Z", "TOTAL"),] %>% 
  .[,activity:=NULL] %>% 
  na.omit() %>%
  dcast(...~sto, value.var = "obs_value") %>% 
  .[,EMP_POP := round(EMP * 100 / POP, digits = 0)] %>% 
  .[,!c("EMP", "POP"),with=FALSE] %>% 
  melt(measure.vars = c("EMP_POP"),
       variable.name = "sto",
       value.name = "obs_value") %>% 
  .[,activity:="_Z"]


# all together in sto
df <- rbindlist(list(df, D1, B1G, B1G_per, HW_PS, EMP_POP), use.names = TRUE)

# Calculating additional units----

# share in NACE

df[, share_nace := obs_value * 100 / obs_value[activity == "TOTAL"], 
   by = .(ref_area, type, sto, time_period)]

# share in country

df[, share_nat := obs_value * 100 / obs_value[NUTS == "0"], by = .(type, sto, activity, time_period)]

# time mean
df[, time_mean := obs_value * 100 / mean(obs_value, na.rm = TRUE), by = .(ref_area, type, sto, activity)]

df <- df %>%
  melt(measure.vars = c("obs_value", "share_nace", "share_nat", "time_mean"),
       variable.name = "unit_measure",
       value.name = "obs_value") %>% 
  dcast(...~type, value.var = "obs_value") %>%   
  .[,rev:= round(T - V)] %>%
  .[,revp:= rev * 100 / V] %>% 
  melt(measure.vars = c("T", "V", "rev", "revp"),
       variable.name = "type",
       value.name = "obs_value") %>% 
  na.omit()

# growth rates
temp <- copy(df) %>%
  .[type %in% c("T","V") & unit_measure == "obs_value",] %>% 
  .[order(time_period),growth := round((obs_value - lag(obs_value)) * 100 / lag(obs_value), digits = 1),
    by = c("type", "ref_area", "activity", "sto", "unit_measure")] %>%   
  .[,!c("obs_value", "unit_measure"),with=FALSE] %>% 
  setnames("growth", "obs_value") %>% 
  .[,unit_measure:= "growth"]


df <- rbindlist(list(df, temp), use.names = TRUE)

### Data for shiny----

df <- df[NUTS != "1"]

df_dotplot <- as.data.table(df) %>%
  dcast(...~type, value.var = "obs_value") 

df_scatter <- df_dotplot

df_scatterb <- df_scatter %>% 
  select(-V,-rev,-revp) %>% 
  filter(unit_measure %in% c("obs_value", "growth") &
  sto %in% c("POP", "EMPhw", "D1", "P51G", "SALhw", "ESEhw", "B1G", "EMP", "SAL", "ESE")) %>% 
  pivot_wider(names_from = unit_measure,
        values_from = T)
  

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
                 checkboxInput(inputId = "freey", "Free Y axis", value = FALSE),
                 checkboxInput(inputId = "leg", "Show Legend", value = FALSE),
                 selectInput("group", "Group by:",
                             choices = c("NUTS","ref_area", "activity", "sto", "type"),
                             selected = "ref_area",
                             multiple = FALSE
                 ),
                 selectInput("colour", "Colour by:",
                             choices = c("NUTS","ref_area", "activity", "sto", "type"),
                             selected = "NUTS",
                             multiple = FALSE
                 ),
                 selectInput("sto", "NA item:",
                             choices = unique(df$sto),
                             selected = "B1G",
                             multiple = TRUE
                 ),
                 selectInput("facet", "Small multiples:",
                             choices = c("ref_area", "activity", "sto", "type", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("NUTS", "NUTS level:",
                             choices = c("0", "2", "3"),
                             selected = "2",
                             multiple = TRUE
                 ),
                 selectInput("activity", "NACE:",
                             choices = unique(df$activity),
                             selected = "TOTAL",
                             multiple = TRUE
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
                 selectInput("yaxis1", "X axis:",
                             choices = c( "T", "V", "rev", "revp"),
                             selected = "T"
                 ),
                 selectInput("xaxis1", "Y axis:",
                             choices = c( "ref_area", "activity", "sto"),
                             selected = "ref_area"
                 ),
                 selectInput("colour1", "Colour by:",
                             choices = c("ref_area", "activity", "sto", "unit_measure","type"),
                             selected = "ref_area"
                 ),
                 selectInput("sto1", "NA item:",
                             choices = unique(df_dotplot$sto),
                             selected = "B1G_EMP",
                             multiple = TRUE
                 ),
                 selectInput("facet1", "Small multiples:",
                             choices = c("ref_area", "activity", "sto", "time_period", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("NUTS1", "NUTS level:",
                             choices = c("0", "2", "3"),
                             selected = "2",
                             multiple = TRUE
                 ),
                 selectInput("activity1", "NACE:",
                             choices = unique(df_dotplot$activity),
                             selected = "TOTAL",
                             multiple = TRUE
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
                             min = min(df_scatter$time_period), max = max(df_scatter$time_period),
                             value = c(min(df_scatter$time_period), max = max(df_scatter$time_period)),
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
                             choices = c("ref_area", "activity", "sto", "unit_measure"),
                             selected = "ref_area"
                 ),
                 selectInput("sto2", "NA item:",
                             choices = unique(df_scatter$sto),
                             selected = "B1G",
                             multiple = TRUE
                 ),
                 selectInput("facet2", "Small multiples:",
                             choices = c("ref_area", "activity", "sto", "time_period", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("NUTS2", "NUTS level:",
                             choices = c("0", "2", "3"),
                             selected = "2",
                             multiple = TRUE
                 ),
                 selectInput("activity2", "NACE:",
                             choices = unique(df_scatter$activity),
                             selected = "TOTAL",
                             multiple = TRUE
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
    
    tabPanel("Scatter plot 2",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("time_period2b", "Time range",
                             min = min(df_scatterb$time_period), max = max(df_scatterb$time_period),
                             value = c(min(df_scatterb$time_period), max = max(df_scatterb$time_period)),
                             step = 1,
                             round = TRUE
                 ),
                 checkboxInput(inputId = "freexyb", "Free X and Y axis", value = TRUE),
                 checkboxInput(inputId = "leg2b", "Show Legend", value = FALSE),
                 selectInput("colour2b", "Colour by:",
                             choices = c("ref_area", "activity", "sto"),
                             selected = "ref_area"
                 ),
                 selectInput("sto2b", "NA item:",
                             choices = unique(df_scatterb$sto),
                             selected = "B1G",
                             multiple = TRUE
                 ),
                 selectInput("facet2b", "Small multiples:",
                             choices = c("ref_area", "activity", "sto", "time_period"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("NUTS2b", "NUTS level:",
                             choices = c("0", "2", "3"),
                             selected = "2",
                             multiple = TRUE
                 ),
                 selectInput("activity2b", "NACE:",
                             choices = unique(df_scatterb$activity),
                             selected = "TOTAL",
                             multiple = TRUE
                 ),
                 selectInput("ref_area2b", "Regions:",
                             choices = unique(df_scatterb$ref_area),
                             selected = unique(df_scatterb$ref_area),
                             multiple = TRUE
                 )
               ),
               
               
               mainPanel(plotlyOutput("scatterPlotb", width = "100%", height = "800px")),
               
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
          selectInput("NUTS5", "NUTS level:",
                      choices = c("0", "2", "3"),
                      selected = "2",
                      multiple = TRUE
          ),
          selectInput("activity5", "NACE:",
                      choices = unique(df_data$activity),
                      selected = "TOTAL",
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
  regions <- reactive({
    filter(df, NUTS == input$NUTS)
  })
  
  observeEvent(regions(), {
    choices <- unique(regions()$ref_area)
    updateSelectInput(inputId = "ref_area", choices = choices,selected=choices) 
  })

  df_filter <- reactive({df %>%
      filter(ref_area %in% input$ref_area &
               time_period >= input$time_period[1] &
               time_period <= input$time_period[2] &
               NUTS %in% input$NUTS &
               sto %in% input$sto &
               activity %in% input$activity &
               type %in% input$type &
               unit_measure %in% input$unit_measure)})
  
  output$linePlot <- renderPlotly({
    # filter data set with user input
    
    # Build  plot
    p <- ggplot(df_filter(), aes(time_period, obs_value, group=!!parse_expr(input$group),colour = !!parse_expr(input$colour),  label = label)) +
      geom_line(size = 0.8)+
      theme_regacc_line+
      scale_colour_luis()+
      scale_y_continuous(breaks = pretty_breaks(3), labels = label_number(),expand=c(0,0.2)) +
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0,0.2))
    
    
    # legend
    
    if (input$leg == FALSE) {
      p <- p + theme(legend.position = "none")
    }    else    {
      p <- p + theme(legend.position = "bottom")
    }
    # y axis from checkbox
    if (input$freey == FALSE) {
      p <- p + facet_wrap(~ get(input$facet))
    }    else    {
      p <- p + facet_wrap(~ get(input$facet), scales = "free_y")
    }
    
    # pass the plot to ggplotly and choose dimensions
    ggplotly(p)%>% config(displayModeBar = F)#, width = 800, height = 800)
  } )
  
  regions1 <- reactive({
    filter(df_dotplot, NUTS == input$NUTS1)
  })
  
  observeEvent(regions1(), {
    choices <- unique(regions1()$ref_area)
    updateSelectInput(inputId = "ref_area1", choices = choices,selected=choices) 
  })

    df_filter1 <- reactive(df_dotplot %>%
                           filter(ref_area %in% input$ref_area1 &
                                    time_period >= input$time_period1[1] &
                                    time_period <= input$time_period1[2] &
                                    NUTS %in% input$NUTS1 &
                                    sto %in% input$sto1 &
                                    activity %in% input$activity1 &
                                    unit_measure %in% input$unit_measure1))
  
  output$dotPlot <- renderPlotly({
    

    
    # Build  plot
    p1 <- ggplot(df_filter1(), aes(x = fct_reorder(!!parse_expr(input$xaxis1), !!parse_expr(input$yaxis1)), y = !!parse_expr(input$yaxis1), colour = !!parse_expr(input$colour1), label = label)) +
      geom_point(size= 2) +
      coord_flip() +
      theme_regacc_scatter +
      scale_colour_luis()+
      scale_y_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1))
    
    
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
  
  regions2 <- reactive({
    filter(df_scatter, NUTS == input$NUTS2)
  })
  
  observeEvent(regions2(), {
    choices <- unique(regions2()$ref_area)
    updateSelectInput(inputId = "ref_area2", choices = choices,selected=choices) 
  })
  
  df_filter2 <- reactive({df_scatter %>%
      filter(ref_area %in% input$ref_area2 &
               time_period >= input$time_period2[1] &
               time_period <= input$time_period2[2] &
               NUTS %in% input$NUTS2 &
               sto %in% input$sto2 &
               activity %in% input$activity2 &
               unit_measure %in% input$unit_measure2)})
  
  output$scatterPlot <- renderPlotly({
 
    
    # Build  plot
    p2 <- ggplot(df_filter2(), aes(!!parse_expr(input$xaxis2), !!parse_expr(input$yaxis2), colour = !!parse_expr(input$colour2), label = label)) +
      geom_point(size= 2) +
      theme_regacc_scatter +
      scale_colour_luis() +
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1))+
      scale_y_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1))
    
    
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
  regions2b <- reactive({
    filter(df_scatterb, NUTS == input$NUTS2b)
  })
  
  observeEvent(regions2b(), {
    choices <- unique(regions2b()$ref_area)
    updateSelectInput(inputId = "ref_area2b", choices = choices,selected=choices) 
  })
   
  df_filter2b <- reactive({df_scatterb %>%
      filter(ref_area %in% input$ref_area2b &
               time_period >= input$time_period2b[1] &
               time_period <= input$time_period2b[2] &
               NUTS %in% input$NUTS2b &
               sto %in% input$sto2b &
               activity %in% input$activity2b) })
  
  output$scatterPlotb <- renderPlotly({
    
    
    # Build  plot
    p2b <- ggplot(df_filter2b(), aes(obs_value, growth, colour = !!parse_expr(input$colour2b), label = label)) +
      geom_point(size= 2) +
      theme_regacc_scatter +
      scale_colour_luis() +
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1))+
      scale_y_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1))
    
    
    # legend
    
    if (input$leg2b == FALSE) {
      p2b <- p2b + theme(legend.position = "none")
    }    else    {
      p2b <- p2b + theme(legend.position = "bottom")
    }
    
    
    # y axis from checkbox
    if (input$freexyb == FALSE) {
      p2b <- p2b + facet_wrap(~ get(input$facet2b))
    }  else   {
      p2b <- p2b + facet_wrap(~ get(input$facet2b), scales = "free")
    }
    
    # pass the plot to ggplotly and choose dimensions
    ggplotly(p2b)%>% config(displayModeBar = F)#, width = 800, height = 800)
  } )

  
  # prepare data frame for table (only values, no derived indicators)
  regions5 <- reactive({
    filter(df_data, NUTS == input$NUTS5)
  })
  
  observeEvent(regions5(), {
    choices <- unique(regions5()$ref_area)
    updateSelectInput(inputId = "ref_area5", choices = choices,selected=choices) 
  })
  df_filter5 <- reactive({df_data %>%
      filter(ref_area %in% input$ref_area5 &
               NUTS %in% input$NUTS5 &
               type %in% input$type5 &
               unit_measure %in% input$unit_measure5 &
               activity %in% input$activity5 &
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


