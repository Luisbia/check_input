ths_int<- 2
ths_per<- 0.1 

df_dt<-list.files(path="data/denodo",
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
  filter(type=="T")

temp<- df_dt %>% 
  filter(table_identifier=="T1200" & unit_measure=="PS" & sto!="POP") %>% 
  pivot_wider(names_from = sto,
        values_from = obs_value) %>% 
  mutate(SELF=EMP-SAL) %>% 
pivot_longer(cols=c(EMP,SAL,SELF),
             values_to="obs_value",
             names_to="sto") %>% 
  filter(activity %in% c("BTE","C")) %>% 
  pivot_wider(names_from = activity,
        values_from = obs_value) %>% 
  mutate(`BTE-C`=BTE-C) %>% 
  filter(`BTE-C`<0) %>% 
  show_in_excel()

temp<- df_dt %>% 
  filter(table_identifier=="T1200" & unit_measure=="XDC" & ref_area=="DK011" & activity=="GTI") 

  
regacc_linechart(temp,hor=time_period,ver=obs_value,grp=ref_area,clr=ref_area, leg=FALSE)+
  annotate("text",x=2018,y=130000, label="Big increase in 2021\nin GVA of activity GTI\n for DK011")

ggsave("notes/DK011_B1G_GTI_2021.png")
