# Figure 5

library(tidyverse)

dsn <- "/home/.../diretório"
setwd(dsn)
 
tab_uni_juve <-read.csv("randomforest_10%_juvenis.csv") %>%  as_tibble()
tab_uni_adul <-read.csv("randomforest_10%.csv") %>%  as_tibble()

# organize data

tab_uni_juve <- tab_uni_juve %>%  mutate(stage= "juvenile")
tab_uni_adul <- tab_uni_adul %>%  mutate(stage= "adult")

tab_uni <- bind_rows(tab_uni_juve, tab_uni_adul)

tab_uni_grp <- tab_uni %>%  group_by(stage, mes) %>%  summarise(SST= mean(SST), "Chlor-a"= mean(Chlor)*10,
                                                      "Predicted abundance"=mean(predicted)) #%>% 

tab_uni_grp <- tab_uni_grp %>% 
  pivot_longer(
    cols = SST:"Predicted abundance", # as colunas desse intervalo
    values_to = "valor")


tab_uni_org <- tab_uni %>%
               group_by(stage, mes) %>%  
               summarize(sd_sst= sd(SST), SST= mean(SST), "Chlor_a"= mean(Chlor),
                         "Pred_catches"=mean(predicted), SD_catches=sd(predicted), 
                          sd_chlor=sd(Chlor)) 

# juveniles plots ################################################################

tab_juvenile <- tab_uni_org %>%  filter(stage=="juvenile")

plot_sst_jv <- tab_juvenile %>%  mutate(lower=SST-sd_sst, upper=SST+sd_sst) %>% 
  ggplot() +
  geom_linerange(aes(ymin=lower, ymax=upper, x=factor(mes)), size=0.8, color="#74a9cf") +
  geom_line(aes(x=mes, y=SST),colour="#023858", size=1) +
  geom_point(aes(x=mes, y=SST),colour="#023858", size=2) +
  scale_x_discrete() +
  labs(x=NULL, y=NULL, title=NULL, colour=NULL) +
  theme_test() +
  theme( axis.title.x = element_text(size=12),
         axis.title.y = element_text(size=12),
         axis.text.x = element_text(size=10),
         axis.text.y = element_text(size=10))+
  ylim(12, 28)
plot_sst_jv


plot_chlor_jv <- tab_juvenile  %>%  mutate(lower=`Chlor_a`-sd_chlor , upper=`Chlor_a`+sd_chlor ) %>% 
  ggplot() +
  geom_linerange(aes(ymin=lower, ymax=upper, x=factor(mes)),size=0.8, color="#78c679") +
  geom_line(aes(x=mes, y=`Chlor_a`),colour="#238443", size=1) +
  geom_point(aes(x=mes, y=`Chlor_a`), colour="#238443",size=2) +
  scale_x_discrete() +
  labs(x="Month", y=NULL, title=NULL, colour=NULL) +
  theme_test() +
  theme( axis.title.x = element_text(size=12),
         axis.title.y = element_text(size=12),
         axis.text.x = element_text(size=10),
         axis.text.y = element_text(size=10))
plot_chlor_jv

plot_catches_jv <- tab_juvenile  %>%  mutate(lower=Pred_catches-SD_catches , upper=Pred_catches+SD_catches ) %>% 
  ggplot()+
  geom_linerange(aes(ymin=lower, ymax=upper, x=factor(mes)), size=0.8, color="#feb24c") +
  geom_line(aes(x=mes, y=Pred_catches),colour="#fc4e2a", size=1) +
  geom_point(aes(x=mes, y=Pred_catches), colour="#fc4e2a", size=2.2) +
  scale_x_discrete() +
  labs(x=NULL, y=NULL, title="       Juveniles", colour=NULL) +
  theme_test() +
  theme(title = element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  ylim(7, 11)
plot_catches_jv

library(patchwork)

series_juvenis <- plot_catches_jv/plot_sst_jv/plot_chlor_jv

series_juvenis 

# adults plots #################################################################

tab_adult <- tab_uni_org %>%  filter(stage=="adult")

plot_sst_ad <- tab_adult %>%  mutate(lower=SST-sd_sst, upper=SST+sd_sst) %>% 
  ggplot() +
  geom_linerange(aes(ymin=lower, ymax=upper, x=factor(mes)), size=0.8, color="#74a9cf") +
  geom_line(aes(x=mes, y=SST),colour="#023858", size=1) +
  geom_point(aes(x=mes, y=SST),colour="#023858", size=2) +
  scale_x_discrete() +
  labs(x=NULL, y=NULL, title=NULL, colour=NULL) +
  theme_test() +
  theme( axis.title.x = element_text(size=12),
         axis.title.y = element_text(size=12),
         axis.text.x = element_text(size=10),
         axis.text.y = element_text(size=10))+
  ylim(12, 28)
plot_sst_ad


plot_chlor_ad <- tab_adult %>%  mutate(lower=`Chlor_a`-sd_chlor , upper=`Chlor_a`+sd_chlor ) %>% 
  ggplot() +
  geom_linerange(aes(ymin=lower, ymax=upper, x=factor(mes)),size=0.8, color="#78c679") +
  geom_line(aes(x=mes, y=`Chlor_a`),colour="#238443", size=1) +
  geom_point(aes(x=mes, y=`Chlor_a`), colour="#238443",size=2) +
  scale_x_discrete() +
  labs(x="Month", y=NULL, title=NULL, colour=NULL) +
  theme_test() +
  theme( axis.title.x = element_text(size=12),
         axis.title.y = element_text(size=12),
         axis.text.x = element_text(size=10),
         axis.text.y = element_text(size=10))
plot_chlor_ad

plot_catches_ad <- tab_adult %>%  mutate(lower=Pred_catches-SD_catches , upper=Pred_catches+SD_catches ) %>% 
  ggplot()+
  geom_linerange(aes(ymin=lower, ymax=upper, x=factor(mes)), size=0.8, color="#feb24c") +
  geom_line(aes(x=mes, y=Pred_catches),colour="#fc4e2a", size=1) +
  geom_point(aes(x=mes, y=Pred_catches), colour="#fc4e2a", size=2.2) +
  scale_x_discrete() +
  labs(x=NULL, y=NULL, title="          Adults", colour=NULL) +
  theme_test() +
  theme(title = element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  ylim(7, 11)
plot_catches_ad


series_adults <- plot_catches_ad/plot_sst_ad/plot_chlor_ad

series_adults

# Fig 5 ########################################################################

series_adults - series_juvenis 


