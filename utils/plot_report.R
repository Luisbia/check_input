# Heatmap 10001----
plot_heatmap_t1001<- function(x,title){
  ggplotly(t1001 %>%
    filter(sto == x) %>%  
    select(ref_area, sto, activity, time_period, change) %>% 
    na.omit() %>% 
    #group_by(ref_area, sto, activity) %>%
    #mutate(norm = round(obs_value / mean(obs_value, na.rm = TRUE),1)) %>% 
    ggplot(aes(time_period, fct_rev(ref_area), fill = change)) +
    geom_tile() +
    theme_regacc_heatmap+
    theme(axis.text.x = element_text(size =9,angle = 90))+
    scale_fill_gradient(low="#FFCC00", high="#0E47CB") +
      scale_x_continuous(breaks=scales::breaks_pretty(n=3), labels = label_number(accuracy = 1),expand=c(0,0))+
    ggtitle(paste0(title)))}

# Heatmap 1002----
plot_heatmap_t1002<- function(x,title){
   ggplotly(t1002 %>%
    filter(sto == x)  %>%
      select(ref_area,NUTS, sto, activity, time_period, obs_value ) %>% 
      group_by(time_period, sto, activity) %>%
      mutate(share = round(obs_value*100 / obs_value[NUTS =="0"],1)) %>% 
      ungroup() %>% 
      filter(NUTS == "2") %>% 
      na.omit() %>% 
     ggplot(aes(time_period, fct_rev(ref_area), fill = share)) +
    geom_tile() +
    facet_wrap(~activity)+ 
    theme_regacc_heatmap+
      theme(axis.text.x = element_text(size =9,angle = 90))+
      scale_fill_gradient(low="#FFCC00", high="#0E47CB") +
      scale_x_continuous(breaks=scales::breaks_pretty(n=3), labels = label_number(accuracy = 1),expand=c(0,0))+
      ggtitle(paste0(title)))}

# Heatmap 1200----
plot_heatmap_t1200<- function(x, y, title){
     ggplotly(t1200 %>%
    filter(sto == x ) %>%  
      select(ref_area,NUTS, sto, activity, time_period, obs_value ) %>% 
      group_by(time_period, sto, activity) %>%
      mutate(share = round(obs_value*100 / obs_value[NUTS =="0"],1)) %>% 
      ungroup() %>% 
      filter(NUTS == y) %>% 
      na.omit() %>% 
    ggplot(aes(time_period, fct_rev(ref_area), fill = share)) +
    geom_tile() +
    facet_wrap(~activity)+ 
    theme_regacc_heatmap+
    theme(axis.text.x = element_text(size =9,angle = 90))+
      scale_fill_gradient(low="#FFCC00", high="#0E47CB") +
      scale_x_continuous(breaks=scales::breaks_pretty(n=3), labels = label_number(accuracy = 1),expand=c(0,0))+
          ggtitle(paste0(title)))}

  
  # Heatmap 1300----
plot_heatmap_t1300<- function(title){
  ggplotly(t1300 %>%
    select(ref_area, NUTS,sto, time_period, obs_value ) %>% 
      group_by(time_period, sto) %>%
      mutate(share = round(obs_value*100 / obs_value[NUTS =="0"],1)) %>% 
      filter (NUTS =="2") %>% 
      na.omit() %>% 
      ggplot(aes(time_period, fct_rev(ref_area), fill = share)) +
    geom_tile() +
    facet_wrap(~sto) +
    theme_regacc_heatmap+
    theme(axis.text.x = element_text(size =9,angle = 90))+
      scale_fill_gradient(low="#FFCC00", high="#0E47CB") +
      scale_x_continuous(breaks=scales::breaks_pretty(n=3), labels = label_number(accuracy = 1),expand=c(0,0))+
        ggtitle(paste0(title)))}


plot_scatter <- function(title) {
ggplotly(temp %>% 
           ggplot(aes(rev,revp,color=NUTS,label=ref_area))+
           geom_point()+
           facet_wrap(~sto, scales="free")+
           theme_regacc_scatter +
           scale_colour_manual(values= c("#0E47CB","#FFCC00","#AA5F18"))+
           scale_y_continuous( breaks = breaks_pretty(3), labels = label_number())+
           scale_x_continuous( breaks = breaks_pretty(3), labels = label_number())+
           ggtitle(title))
  }
