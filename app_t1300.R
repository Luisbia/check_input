library(rio)
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggiraph)
library(rlang)
library(DT)
library(data.table)
library(scales)
library(dataregacc)
source("utils/palette.R")
source("utils/theme_report.R")


# Data manipulation----
## import files

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
  mutate(NUTS=as.factor(NUTS))

  
t1300<- copy(df) %>%   
  .[table_identifier=="T1300",] %>% 
    .[,.(type,ref_area,NUTS, accounting_entry,sto,time_period,obs_value )] %>% 
  unite("sto", c (accounting_entry,sto))



# get population
temp <- copy(df) %>%
  .[table_identifier == "T1001" & sto == "POP" & activity == "_Z"] %>% 
  .[,.(type,ref_area,NUTS, sto,time_period,obs_value )]

df <- rbindlist(list(t1300, temp))

#bring labels
NUTS2021<-NUTS_2021 %>% 
  filter(country %in% country_sel) %>% 
  mutate(across(c(NUTS,label),as_factor)) %>% 
  mutate(country=as.character(country)) %>% 
  as.data.table()

df <- merge (NUTS2021, df, by= c("ref_area","NUTS")) %>% 
  .[NUTS %in% c ("0", "2") & label != "Extra-regio",]


# some ratios
df[, per_capita := obs_value * 1000 / obs_value[sto == "POP"], by = list(ref_area, type, time_period)]
df[, nat_per_capita := per_capita * 100 / per_capita[NUTS == "0"], by = list(type, sto, time_period)]
df[, nat_share := obs_value * 100 / obs_value[NUTS == "0"], by = list(type, sto, time_period)]
df[, b5n_share := obs_value * 100 / obs_value[sto == "B_B5N"], by = list(ref_area, type, time_period)]
df[, d1_share := obs_value * 100 / obs_value[sto == "C_D1"], by = list(ref_area, type, time_period)]
df[, time_mean := obs_value * 100 / mean(obs_value), by = list(ref_area, type, sto)]

df<- melt(df,measure.vars = c("obs_value", "per_capita", "nat_per_capita", "nat_share", "b5n_share","d1_share","time_mean"),
          variable.name = "unit_measure",
          value.name = "obs_value") %>% 
  dcast(...~type, value.var = "obs_value") %>% 
  .[,rev:= round(T - V)] %>% 
  .[,revp:= round(rev * 100 / V,1)] %>% 
  melt(measure.vars = c("T", "V", "rev", "revp"),
       variable.name = "type",
       value.name = "obs_value")

# growth rates

temp <- as.data.table(df) %>%
  .[type %in% c("T","V") & unit_measure == "obs_value",] %>% 
  .[order(time_period),
    growth := round((obs_value - lag(obs_value)) * 100 / lag(obs_value), digits = 1),
    by = c("type", "ref_area", "sto", "unit_measure")]  %>% 
  .[,!c("obs_value", "unit_measure"),with=FALSE] %>% 
  setnames("growth", "obs_value") %>% 
  .[,unit_measure:="growth"]

df<- rbindlist(list(df,temp), use.names = TRUE)


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
                  tags$title("Regional Accounts Data Analysis: table 1300")
                ),
  tabsetPanel(
    tabPanel("Line plot",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("time_period", "time_period range",
                             min = min(df$time_period), max = max(df$time_period),
                             value = c(min(df$time_period), max = max(df$time_period)),
                             step = 1,
                             round = TRUE
                 ),
                 checkboxInput(inputId = "freey", "Free Y axis", value = FALSE),
                 checkboxInput(inputId = "leg", "Show Legend", value = FALSE),
                 selectInput("group", "Group by:",
                             choices = c("ref_area","sto", "type"),
                             selected = "ref_area",
                             multiple = FALSE
                 ),
                 selectInput("colour", "Colour by:",
                             choices = c("ref_area", "sto", "type"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("sto", "NA item:",
                             choices = unique(df$sto),
                             selected = "B_B6N",
                             multiple = TRUE
                 ),
                 selectInput("facet", "Small multiples:",
                             choices = c("ref_area", "sto", "type", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("NUTS", "NUTS level:",
                             choices = c("0", "2"),
                             selected = "2",
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
                             multiple = TRUE
                 )
               ),
               
               
        mainPanel(girafeOutput("linePlot", width = "100%", height = "800px")),
          
          )#mainPanel
        ), #tabPanel
    
    tabPanel("Dot plot",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("time1", "time_period range",
                             min = min(df_dotplot$time_period), max = max(df_dotplot$time_period),
                             value = c(min(df_dotplot$time_period), max = max(df_dotplot$time_period)),
                             step = 1,
                             round = TRUE
                 ),
                 checkboxInput(inputId = "freex", "Free X axis", value = TRUE),
                 checkboxInput(inputId = "leg1", "Show Legend", value = FALSE),
                 selectInput("yaxis1", "X axis:",
                             choices = c( "T","V", "rev", "revp"),
                             selected = "T"
                 ),
                 selectInput("colour1", "Colour by:",
                             choices = c("ref_area","sto", "unit_measure"),
                             selected = "sto"
                 ),
                 selectInput("na_item1", "NA item:",
                             choices = unique(df_dotplot$sto),
                             selected = "B_B6N",
                             multiple = TRUE
                 ),
                 selectInput("facet1", "Small multiples:",
                             choices = c("ref_area", "sto", "time_period", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("NUTS1", "NUTS level:",
                             choices = c("0", "2"),
                             selected = "2",
                             multiple = TRUE
                 ),
                 selectInput("unit_measure1", "Unit of the series:",
                             choices = unique(df_dotplot$unit_measure),
                             selected = "nat_per_capita",
                             multiple = TRUE
                 ),
                 selectInput("geo1", "Regions:",
                             choices = unique(df_dotplot$ref_area),
                             selected = unique(df_dotplot$ref_area),
                             multiple = TRUE
                 )
               ),
               
               
               
               mainPanel(girafeOutput("dotPlot", width = "100%", height = "800px")),
               
             )#mainPanel
    ),    #tabPanel
    

    tabPanel("Scatter plot",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("time2", "time_period range",
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
                             choices = c("ref_area", "sto", "unit_measure"),
                             selected = "sto"
                 ),
                 selectInput("na_item2", "NA item:",
                             choices = unique(df_scatter$sto),
                             selected = "B_B6N",
                             multiple = TRUE
                 ),
                 selectInput("facet2", "Small multiples:",
                             choices = c("ref_area", "sto", "time_period", "unit_measure"),
                             selected = "sto",
                             multiple = FALSE
                 ),
                 selectInput("NUTS2", "NUTS level:",
                             choices = c("0", "2"),
                             selected = "2",
                             multiple = TRUE
                 ),
                 selectInput("unit_measure2", "Unit of the series:",
                             choices = unique(df_scatter$unit_measure),
                             selected = "obs_value",
                             multiple = TRUE
                 ),
                 selectInput("geo2", "Regions:",
                             choices = unique(df_scatter$ref_area),
                             selected = unique(df_scatter$ref_area),
                             multiple = TRUE
                 )
               ),
               
               mainPanel(girafeOutput("scatterPlot", width = "100%", height = "800px")),
               
             )#mainPanel
    ),
    # Data----
    tabPanel(
      "Data",
      sidebarLayout(
        sidebarPanel(
          selectInput("na_item3", "NA Item:",
                      choices = unique(df_data$sto),
                      selected = unique(df_data$sto),
                      multiple = TRUE
          ),
          selectInput("type3", "Type:",
                      choices = unique(df_data$type),
                      selected = "T",
                      multiple = TRUE
          ),
          selectInput("unit_measure3", "Unit:",
                      choices = unique(df_data$unit_measure),
                      selected = "obs_value",
                      multiple = TRUE
          ),
          selectInput("geo3", "Regions:",
                      choices = unique(df_data$ref_area),
                      selected = unique(df_data$ref_area),
                      multiple = TRUE)
        ),
        
        mainPanel(DTOutput("data")
                  )))  
    ) #tabsetpanel
)#fluid page


server <- function(input, output) {
  df_filter <- reactive({df %>%
      filter(ref_area %in% input$ref_area &
               time_period >= input$time_period[1] &
               time_period <= input$time_period[2] &
               NUTS %in% input$NUTS &
               sto %in% input$sto &
               type %in% input$type &
               unit_measure %in% input$unit_measure)})
  
  output$linePlot <- renderGirafe({
    # filter data set with user input
    
    # Build  plot
    p <- ggplot(df_filter(), aes(time_period, obs_value, group=!!parse_expr(input$group),colour = !!parse_expr(input$colour),text=paste(label))) +
      geom_line_interactive(aes(tooltip= paste0(ref_area," - ",label,"\n",sto,"\n",type, "\n", unit_measure),data_id=ref_area),size = 0.8)+
      geom_point_interactive(aes(tooltip=obs_value,data_id=ref_area))+
      theme_regacc_line+
      scale_colour_luis()+
      scale_y_continuous(breaks = pretty_breaks(3), labels = label_number(),expand=c(0,0.4))+
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0.1,0.4))
    
      # legend
      
      if (input$leg == FALSE) {
        p <- p + theme(legend.position = "none")
      }    else    {
        p <- p + theme(legend.position = "top")
      }
    # y axis from checkbox
    if (input$freey == FALSE) {
      p <- p + facet_wrap_interactive(vars(get(input$facet)),
                                      labeller = labeller_interactive())+
        theme(strip.text.x = element_text_interactive(),
              strip.text.y = element_text_interactive())
    }    else    {
      p <- p + facet_wrap_interactive(scales="free_y",vars(get(input$facet)),
                                      labeller = labeller_interactive())+
        theme(strip.text.x = element_text_interactive(),
              strip.text.y = element_text_interactive())
    }
    
    # pass the plot to ggplotly and choose dimensions
    girafe(ggobj = p)

  } )
  
  
  df_filter1 <- reactive(df_dotplot %>%
                           filter(ref_area %in% input$geo1 &
                                    time_period >= input$time1[1] &
                                    time_period <= input$time1[2] &
                                    NUTS %in% input$NUTS1 &
                                    sto %in% input$na_item1 &
                                    unit_measure %in% input$unit_measure1))
  
  output$dotPlot <- renderGirafe({
    
    # filter data set with user input
    
    # Build  plot
    p1 <- ggplot(df_filter1(), aes(y = fct_reorder(ref_area, !!parse_expr(input$yaxis1)), x = !!parse_expr(input$yaxis1), colour = !!parse_expr(input$colour1), text = paste(label))) +
      geom_point_interactive(aes(tooltip=paste0(ref_area," - ",label,"\n",time_period,"\n",sto, "\n",unit_measure),data_id=ref_area),size= 2) +
      theme_regacc_scatter +
      scale_colour_luis() +
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0.1,0.4))+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())

    
    
    
    # legend
    
    if (input$leg1 == FALSE) {
      p1 <- p1 + theme(legend.position = "none")
    }    else    {
      p1 <- p1 + theme(legend.position = "right")
    }
    # y axis from checkbox
    if (input$freex == FALSE) {
      p1 <- p1+ facet_wrap_interactive(vars(get(input$facet1)),
                                       labeller = labeller_interactive())+
        theme(strip.text.x = element_text_interactive(),
              strip.text.y = element_text_interactive())
    }    else  {
      p1 <- p1 + facet_wrap_interactive(scales="free_y",vars(get(input$facet1)),
                                        labeller = labeller_interactive())+
        theme(strip.text.x = element_text_interactive(),
              strip.text.y = element_text_interactive())
    }
    
    girafe(ggobj = p1)#, width = 800, height = 800)
  } )
  
 
  df_filter2 <- reactive(df_scatter %>%
                           filter(ref_area %in% input$geo2 &
                                    time_period >= input$time2[1] &
                                    time_period <= input$time2[2] &
                                    NUTS %in% input$NUTS2 &
                                    sto %in% input$na_item2 &
                                    unit_measure %in% input$unit_measure2))
  
  output$scatterPlot <- renderGirafe({
    # filter data set with user input
    
    
    # Build  plot
    p2 <- ggplot(df_filter2(), aes(!!parse_expr(input$xaxis2), !!parse_expr(input$yaxis2), colour = !!parse_expr(input$colour2), text = paste(label))) +
      geom_point_interactive(aes(tooltip=paste0(ref_area," - ",label,"\n",time_period, "\n", sto,"\n", unit_measure),data_id=ref_area),size= 2) +
      theme_regacc_scatter +
      scale_colour_luis() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())+
      scale_x_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0.1,0.4))+
      scale_y_continuous( breaks = pretty_breaks(3), labels = label_number(accuracy = 1),expand=c(0.1,0.4))
    
    # legend
    
    if (input$leg2 == FALSE) {
      p2 <- p2 + theme(legend.position = "none")
    }    else    {
      p2 <- p2 + theme(legend.position = "bottom")
    }
    # y axis from checkbox
    if (input$freexy == FALSE) {
      p2 <- p2+ facet_wrap_interactive(vars(get(input$facet2)),
                                       labeller = labeller_interactive())+
        theme(strip.text.x = element_text_interactive(),
              strip.text.y = element_text_interactive())
    }    else  {
      p2 <- p2 + facet_wrap_interactive(scales="free",vars(get(input$facet2)),
                                        labeller = labeller_interactive())+
        theme(strip.text.x = element_text_interactive(),
              strip.text.y = element_text_interactive())
    }
    girafe(ggobj = p2)#, width = 800, height = 800)
  } )  
  

  df_filter3 <- reactive(df_data %>%
                           filter(ref_area %in% input$geo3 &
                                    sto %in% input$na_item3 &
                                    unit_measure %in% input$unit_measure3 &
                                    type %in% input$type3))
  output$data <- renderDT(
    datatable(df_filter3(),
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
  )
  
  
}

# Run the application

shinyApp(ui = ui, server = server)
