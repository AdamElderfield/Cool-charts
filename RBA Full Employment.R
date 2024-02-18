###########################
# Recreating RBA Plot
###########################
library(ggplot2)
library(tidyverse)
library(glue)


##############
# Chart theme
#############
theme_rba <- theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_text(
                                   colour = "black"
                                    )
      
        
          )

############
# Chart title
############


title_chart <- labs(title = bquote(bold('Full Employment Indicators*')),subtitle =  'Conditions relative to 2000-2004 range')





##############
# DATA
#############

## NEED TO USE LIST?

LF <- readabs::read_abs("6202.0")

LF_CHART <- LF %>% 
  filter( table_no %in% c("6202022"),
           series %in% c("Unemployment rate ;  Persons ;",
                         "Underemployment rate (proportion of labour force) ;  Persons ;",
                         "Unemployment rate ;  Persons ;  15-24 years ;") &
           series_type == "Seasonally Adjusted" &
           date >= "2000-01-01") 


CHART_DATA_2 <- LF_CHART %>% 
  select(series_id,date,value) %>% 
  group_by(series_id) %>% 
  mutate(Loosest = max(value),
         Tightest = min(value),
         P20 =  quantile(value,c(0.2,.8)) %>%
           .[[2]],
         P80 =  quantile(value,c(0.2,.8)) %>%
           .[[1]],
                  Current = last(value),
         Oct_22 = value[which(date=="2022-10-01")]) %>%
  
  select(-value) %>% 
  filter(date == last(date)) %>% 
  select(-date) %>% 
  mutate(series_id = paste(series_id," ",round(Loosest,1))) %>% 
   gather(Var,Val, -series_id) %>% 
  group_by(series_id) %>%
  mutate(Val =Val/Val[Var == "Loosest"]-1) %>%
  mutate(Val = ifelse(Val<0,Val*-1,Val)) %>% 
  spread(Var,Val) %>%
 
  group_by(series_id) %>% 
  mutate(       P20_P80 = P80-P20,
                Base = P20-Loosest,
                Top = Tightest-P80,
                Current = Current-Loosest,
                Oct_22 = Oct_22-Loosest
  ) %>%
  select(series_id,Base,P20_P80,Top) %>% 
  
  gather(Var,Val, -series_id) %>% 
  group_by(series_id) %>% 
  mutate(Val = Val/sum(Val))


CHART_DATA_Line <- LF_CHART %>% 
  select(series_id,date,value) %>% 
  group_by(series_id) %>% 
  mutate(Loosest = max(value),
         Tightest = min(value),
         P20 =  quantile(value,c(0.2,.8)) %>%
           .[[2]],
         P80 =  quantile(value,c(0.2,.8)) %>%
           .[[1]],
         Current = last(value),
         Oct_22 = value[which(date=="2022-10-01")]) %>%
  select(-value) %>% 
  filter(date == last(date)) %>% 
  select(-date) %>% 
  mutate(series_id = paste(series_id," ",round(Loosest,1))) %>% 
    gather(Var,Val, -series_id) %>% 
  group_by(series_id) %>%
  mutate(Val =Val/Val[Var == "Loosest"]-1) %>% 
  mutate(Val = ifelse(Val<0,Val*-1,Val)) %>% 
  spread(Var,Val) %>% 
  group_by(series_id) %>% 
  mutate(       P20_P80 = P80-P20,
                Base = P20-Loosest,
                Top = Tightest-P80,
                Current = Current-Loosest,
                Oct_22 = Oct_22-Loosest
  ) %>%
  select(series_id,Base,P20_P80,Top, Current, Oct_22) %>% 
  group_by(series_id) %>% 
  mutate(Current = Current/(Base+P20_P80+Top),
         Oct_22 = Oct_22/(Base+P20_P80+Top)) %>% 
  
gather(Var,Val, -series_id) 
  


series_ids <- LF_CHART$series_id %>% unique()
new_names <- c("Underemployment rate","Unemployment rate","Youth unemployment rate")

for(i in seq_along(series_ids)){
  CHART_DATA_2$series_id <- gsub(series_ids[i],new_names[i],CHART_DATA_2$series_id)
  CHART_DATA_Line$series_id <- gsub(series_ids[i],new_names[i],CHART_DATA_Line$series_id)
}

############
# Build chart
############


p <- ggplot(CHART_DATA_2  %>% 
      #  filter(series_id == "A85255725J") %>%
        filter(Var %in%  c("Base","P20_P80","Top")) %>% 
        mutate(Var = factor(Var, levels = c("Top", "P20_P80", "Base"))),
      aes(label = series_id)
         )+
  
   geom_bar(
     aes(x = series_id,y=Val, fill = Var), width = 0.15,
     stat = "identity",
   ) +
  
  geom_line(data =CHART_DATA_Line%>% 
              
              filter(Var %in% c("Current","Oct_22")) ,
    aes(
      x =series_id, y = Val), color = "black" 
    )+
  geom_point(data =CHART_DATA_Line%>% 
               
               filter(Var %in% c("Current","Oct_22")),
    aes(
      x = series_id, y = Val ,
       group = Var
      
    ),color = c("orange","orange","orange","blue","blue","blue"),
    shape = c("circle","circle","circle","circle","circle","circle"),
    size = 4,
    
    )+
  coord_flip()+
  scale_fill_manual(values = c("white","grey","white"))+theme_rba+
  title_chart+            # Base plot
  theme(plot.margin = unit(c(1,3,1,1), "lines"))   # Make room for the grob

for (i in seq_along(series_ids))  {
  h <- series_ids[i]
  name_in_chart <- unique(CHART_DATA_2$series_id)
  
  label_text <- LF_CHART %>% 
    filter(series_id == h) %>% 
    select(series_id,date,value) %>%
    mutate(Val = last(value)) %>% 
    .$Val  # NEEDS TO BE "Tightest
  
  
  p <- p + annotation_custom(
    grob = textGrob(label = round(label_text[1],1), hjust = 0, gp = gpar(cex = 0.75)), # gp = gpar(cex = 1.5)
    ymin = 1.15,      # Vertical position of the textGrob
    ymax = 1,
    xmin = i,         # Note: 1,1 will be the bottom most bar, 2,2 the bar above it
    xmax = i)
}    

# Code to override clipping
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
