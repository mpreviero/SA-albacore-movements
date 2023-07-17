## Fig 2 plots 
# calculates the average ALB size in (Task 2 sizes) by areas in the South Atlantic
# convert captures to number of individuals and weight

library(tidyverse)
dsn <- "/home/usuario/Documentos/pesquisas/TRIATLAS/Paper 2 TRIATLAS/analises/diretório"
setwd(dsn)

alb_sz <- read.csv("task2_sz_alb_paper2.csv")

# divides the data frame into 8 areas
# LL - longline
alb_sz_LL_areas <- alb_sz %>%  mutate(areas = case_when(Lat >= 0 & Lon <= -20 ~ "area 1",
                                                        Lat >= 0 & Lon > -20 ~ "area 2",
                                                        Lat < 0 & Lat >= -20 & Lon <= -15 ~ "area 3",
                                                        Lat < 0 & Lat >= -20 & Lon > -15 ~ "area 4",
                                                        Lat < -20 & Lat >= -30 & Lon <= -10 ~ "area 5",
                                                        Lat < -30 & Lat >= -45 & Lon <= -35 ~ "area 6",
                                                        Lat < -30 & Lat >= -45 & Lon > -35 & Lon <= -10 ~ "area 7",
                                                        Lat < -20 & Lat >= -45 & Lon > -10 ~ "area 8" ,
                                                        TRUE ~"none"))

#write.csv(alb_sz_LL_areas, "alb_sz_LL_areas.csv", row.names = FALSE)
alb_sz_LL_areas_csv <- alb_sz_LL_areas %>%  dplyr::select("Cod_qdd","GeoStrata","QuadID","Lat","Lon","areas")  

sz_freq <- alb_sz_LL_areas %>% dplyr::select("classe"="ClassFrq", "Num", "areas")

sz_freq <-sz_freq %>%  group_by(areas, classe) %>%  summarise(num=sum(Num))

# get the average length by area

# adds a column repeating the lengths as many times as they were measured
data_hist <- sz_freq %>% 
  uncount(.remove = FALSE, weights = num) %>% 
  mutate(tam_med= mean(classe), tam_ref = 90, 
         maturity = if_else(tam_med > tam_ref, "A", "J"))
data_hist$maturity <- as.factor(data_hist$maturity)
data_hist %>% as_tibble()

# number of measured individuals by area
count(data_hist,areas)

# histogram to each area: change the area number to obtain the desired histogram:
data_hist %>% filter(areas=="area 8") %>% 
  ggplot(aes(x=classe))+
  geom_density(size = 1, fill="grey3") +
  geom_vline(aes(xintercept = tam_ref, colour="L50"), size=3)+
  # geom_vline(aes(xintercept = tam_med, colour="Average size"),size=3)+
  annotate(geom="text", x=120, y=0.04, label="Area 8 \n n=1.107.550",    # change label
           color="black", size=7) +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       title = NULL) +
  scale_color_manual(values = c("L50" = "red", "Average size" = "green")) +
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=22, face="bold"),
        axis.text.y = element_text(size=22, face="bold"))


