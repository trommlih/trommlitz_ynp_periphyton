install.packages("ggplot2")

library(tidyverse)
library(readxl) 
library(lubridate)
library(dplyr)

packageVersion("ggplot2")
install.packages(
  "ggpattern",
  repos = c("https://trevorld.r-universe.dev", "https://cloud.r-project.org"))
library(ggplot2)
library(ggpattern)

setwd("~/Library/CloudStorage/Box-Box/Warren Lab Research Folder/Undergrad theses/Hannah Trommlitz - YNP Algal Community")
#setwd("C:/Users/swartza/Box Sync/Warren Lab Research Folder/Undergrad theses/Hannah Trommlitz - YNP Algal Community/")


new_data_raw_species<- read_excel("YNP algal data combined_corrected abundance_8-8-23_DW_HT.xlsx", sheet = "abund w-o EFBT-T4 and live diat")

new_data_raw_species <- new_data_raw_species[, 1:25] 
print(colnames(new_data_raw_species))
new_data_raw_species<-new_data_raw_species %>% rename( "No.Individuals" = "est number of INDIVIDUALS in the sample analyized by rithron (from within the  proportion examined)")
new_data_raw_species<-new_data_raw_species %>% rename( "No.heterocysts" = "# heterocysts per cm2 (scaled to total area sampled with lobe sampler)") #wasnt sure which one I should use but for some reason this one made sense
new_data_raw_species$Date<- as.Date("1970-01-01") + new_data_raw_species$Date - 25569 
new_data_raw_species<- filter(new_data_raw_species, Stream %in% c("Ant","Lost","EFBT","Geode","Rose","JSP")) #do i just do all?? who knows we're doing it tho

new_data_raw_species$Stream.Year<- paste(new_data_raw_species$Stream, new_data_raw_species$year, sep=".")           
new_data_raw_species$Stream.Year.Season<- paste(new_data_raw_species$Stream.Year, new_data_raw_species$`time period`, sep=".")  
new_data_raw_species$Year.Season<- paste(new_data_raw_species$year, new_data_raw_species$`time period`, sep=".")  

new_data_summary_by_Stream_period_species<- new_data_raw_species %>% 
  group_by(Stream, year, `time period`,Year.Season,  Stream.Year.Season , Vegetation) %>% 
  summarize(Sp.Richness = n(),                  
            Sp.TotalNum = sum( No.Individuals),
            TotalNum.Heterocysts = sum(No.heterocysts)) 

data_raw_genus<- new_data_raw_species %>% 
  group_by(Stream, year, `time period`, Stream.Year.Season ,Year.Season, Vegetation, Genus) %>% 
  summarize(No.Individuals_Genus = sum( No.Individuals))

data_summary_by_Stream_period_genus<- data_raw_genus %>% 
  group_by(Stream, year, `time period`, Stream.Year.Season ,Year.Season, Vegetation) %>% 
  summarize(Genus.Richness = n(),                     #count of the unique species (not including their amts)
            Genus.TotalNum = sum( No.Individuals_Genus))

data_summary_by_Stream_period_genus$`time period`<- factor(data_summary_by_Stream_period_genus$`time period`, levels= c("Early-summer","Mid-summer","Late-summer"))
#write.csv(data_summary_by_Stream_period_genus, "Data_Summary_Genus.csv")


#I have zero clue if this works how its supposed to - got it from Chat
#new_data_raw_species$Lowest_Taxon <- apply(new_data_raw_species[, c("Species", "Genus")], 1, function(x) {
  # Return the first non-NA from right to left (Species → Phylum)
  #taxon_levels <- rev(x)
  #first_non_na <- taxon_levels[which(!is.na(taxon_levels))][1]
  #return(first_non_na)
#})

#new_data_summary_by_Stream_period_species <- new_data_raw_species %>%
 # group_by(Lowest_Taxon) %>%
 # summarise(Total_Count = sum(Count, na.rm = TRUE))



#GPP
GPP_data <- read_excel("YNP_GPP_HT.xlsx", sheet = 1)


#Standing stocks
chla_AFDM_data_all <- read_excel("YNP_chlaAFDM.xlsx", sheet = 1) 
chla_AFDM_data <- chla_AFDM_data_all [-c(21:25, 27, 28), ] # Removing sites that don't also have community data in the same time period







#all the sites available with standing stocks
chla_AFDM_data_all$year.season<- paste(chla_AFDM_data_all$year, chla_AFDM_data_all$`time period`, sep=".")  
chla_AFDM_data$year.season<- paste(chla_AFDM_data$year, chla_AFDM_data$`time period`, sep=".")  





# Make sure year.season is a factor with the desired order
chla_AFDM_data <- chla_AFDM_data %>%
  mutate(year.season = factor(year.season,
                         levels = c("2019.Mid-summer","2019.Late-summer",
                                    "2020.Early-summer","2020.Mid-summer")))
###############getting means and merging data - Combining all Sites ################################### 


# Remove any groups to avoid grouping issues
chla_AFDM_data <- chla_AFDM_data %>% ungroup()

# Add missing combinations of Stream x year.season
chla_AFDM_data <- chla_AFDM_data %>%
  complete(Stream, year.season, fill = list(chlamgperm2 = 0))


# Add missing combos
chla_AFDM_data <- chla_AFDM_data %>%
  complete(Stream, year.season, fill = list(chlamgperm2 = 0))


ggplot(chla_AFDM_data, aes(x = Stream, y = `chla mgperm2`, fill = year.season, pattern = year.season)) +
  geom_bar_pattern(stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.9,
           pattern_fill = "black",
           pattern_colour = "black",
           pattern_density = 0.05,
           pattern_spacing = 0.02)+
 
   geom_errorbar(aes(ymin = `chla mgperm2` - SEchlamgperm2,
                    ymax = `chla mgperm2` + SEchlamgperm2),
                position = position_dodge(width = 0.8),
                width = 0.2,                # width of error bar caps
                linewidth = 0.6) +          # thickness of error bar lines
  scale_fill_manual(values = c("goldenrod1", "firebrick2","springgreen4", "royalblue3"),
    breaks = c("2019.Mid-summer","2019.Late-summer","2020.Early-summer","2020.Mid-summer")) +
 
   labs(x = "Stream",
       y = "Mean Chlorophyll-a (mg/m²)", 
       fill = "Time Period", 
       pattern = "year.season")+
 
   scale_pattern_manual(values = c(
      "2019.Mid-summer" = "crosshatch",
      "2019.Late-summer" = "crosshatch", 
      "2020.Early-summer" = "stripe",
      "2020.Mid-summer" = "stripe")) +
  
  labs(x = "Stream",
       y = "Mean Chlorophyll-a (mg/m²)", 
       fill = "Time Period",
       pattern = "year.season") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))
  




# Remove any groups to avoid grouping issues
chla_AFDM_data <- chla_AFDM_data %>% ungroup()

# Add missing combinations of Stream x year.season
chla_AFDM_data <- chla_AFDM_data %>%
  complete(Stream, year.season, fill = list(afdmmgperm2 = 0))


# Add missing combos
chla_AFDM_data <- chla_AFDM_data %>%
  complete(Stream, year.season, fill = list(afdmmgperm2 = 0))


ggplot(chla_AFDM_data, aes(x = Stream, y = `afdmmgperm2`, fill = year.season)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  geom_errorbar(aes(ymin = `afdmmgperm2` - SEafdmmgperm2,
                    ymax = `afdmmgperm2` + SEafdmmgperm2),
                position = position_dodge(width = 0.8),
                width = 0.2,                # width of error bar caps
                linewidth = 0.6) +          # thickness of error bar lines
  scale_fill_manual(
    values = c("goldenrod1", "firebrick2","springgreen4", "royalblue3"),
    breaks = c("2019.Mid-summer","2019.Late-summer","2020.Early-summer","2020.Mid-summer")) +
  labs(x = "Stream",
       y = "Mean Ash Free Dry Mass (mg/m²)", 
       fill = "Time Period") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))







#Diversity Metrics SPECIES
Proportions<-merge(new_data_raw_species, new_data_summary_by_Stream_period_species, by = c("Stream","year", "time period"))
Proportions$Proportion<- Proportions$No.Individuals / Proportions$Sp.TotalNum            ####  Proportion is the same as Relative Abundance

Proportions$P_lnP<- Proportions$Proportion * log(Proportions$Proportion)
Proportions$p2<- (Proportions$Proportion)^2
Proportions$Simp<-(Proportions$No.Individuals*(Proportions$No.Individuals-1))/(Proportions$Sp.TotalNum*(Proportions$Sp.TotalNum-1))

Diversity_metrics_all<-Proportions %>% group_by(Stream, year, `time period`,)%>% summarize(Sum_P_lnP = sum(P_lnP),
                                                                                                   Sum_simp= sum(Simp),
                                                                                                   Total= mean(Sp.TotalNum))
Diversity_metrics_all$Shannon<- (Diversity_metrics_all$Sum_P_lnP) *(-1)  
Diversity_metrics_all$Simpsons<- 1-Diversity_metrics_all$Sum_simp
Diversity_metrics <- Diversity_metrics_all [-c(5), ] #why is that extra "," necessary?? #removing crystal
write.csv(Diversity_metrics, "Diversity_Metrics.csv")

#evenness_genus <- Diversity_metrics_Genus$Shannon / log(84)  #Allison changed this to 84. 
#n_distinct(unique(data_raw_genus$Genus))
#Diversity_metrics_Genus$Evenness<- evenness_genus 

#Diversity metrics with all sites
Diversity_metrics_all_rich <- left_join(Diversity_metrics_all, new_data_summary_by_Stream_period_species, by= c('Stream', 'year', 'time period'))


#DIVERSITY METRICS GENERA 
Proportions<-merge(data_raw_genus, data_summary_by_Stream_period_genus, by = c("Stream","year", "time period"))
Proportions$Proportion<- Proportions$No.Individuals_Genus / Proportions$Genus.TotalNum            ####  Proportion is the same as Relative Abundance

Proportions$P_lnP<- Proportions$Proportion * log(Proportions$Proportion)
Proportions$p2<- (Proportions$Proportion)^2
Proportions$Simp<-(Proportions$No.Individuals_Genus*(Proportions$No.Individuals_Genus-1))/(Proportions$Genus.TotalNum*(Proportions$Genus.TotalNum-1))

Diversity_metrics_all<-Proportions %>% group_by(Stream, year, `time period`,)%>% summarize(Sum_P_lnP = sum(P_lnP),
                                                                                           Sum_simp= sum(Simp),
                                                                                           Total= mean(Genus.TotalNum))
Diversity_metrics_all$Shannon<- (Diversity_metrics_all$Sum_P_lnP) *(-1)  
Diversity_metrics_all$Simpsons<- 1-Diversity_metrics_all$Sum_simp
Diversity_metrics <- Diversity_metrics_all [-c(5), ] #why is that extra "," necessary?? #removing crystal
write.csv(Diversity_metrics, "Diversity_Metrics.csv")

#evenness_genus <- Diversity_metrics_Genus$Shannon / log(84)  #Allison changed this to 84. 
#n_distinct(unique(data_raw_genus$Genus))
#Diversity_metrics_Genus$Evenness<- evenness_genus 

#Diversity metrics with all sites
Diversity_metrics_all_rich <- left_join(Diversity_metrics_all, data_summary_by_Stream_period_genus, by= c('Stream', 'year', 'time period'))




# Remove any groups to avoid grouping issues
chla_AFDM_data <- chla_AFDM_data %>% ungroup()

# Add missing combinations of Stream x year.season
chla_AFDM_data <- chla_AFDM_data %>%
  complete(Stream, year.season, fill = list(Genus.Richness = 0))

# Remove any groups to avoid grouping issues
data_summary_by_Stream_period_genus <- data_summary_by_Stream_period_genus %>% ungroup()

# Add missing combinations of Stream x year.season
data_summary_by_Stream_period_genus <- data_summary_by_Stream_period_genus %>%
  complete(Stream, Year.Season, fill = list(Genus.Richness = 0))



#SPECIES RICHNESS BAR GRAPH 
ggplot(chla_AFDM_data, aes(x = Stream, y = Sp.Richness, fill = year.season)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  scale_fill_manual(
    values = c("yellowgreen", "indianred","cornflowerblue", "forestgreen"),
    breaks = c("2019.Mid-summer","2019.Late-summer","2020.Early-summer","2020.Mid-summer") ) +
  labs(x = "Stream",
       y = "Richness", 
       fill = "Time Period") +
  theme_minimal() +                       # start with minimal theme
  theme(
    panel.background = element_rect(fill = "white"),  # white background
    panel.grid = element_blank(),                     # remove all grid lines
    axis.text.x = element_text(angle = 45, hjust = 1)) # rotate x labels)


#GENERA RICHNESS 
ggplot(data_summary_by_Stream_period_genus, aes(x = Stream, y = Genus.Richness, fill = Year.Season)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9), width = 0.8) +
  scale_fill_manual(
    values = c("goldenrod1", "firebrick2","springgreen4", "royalblue3"),
    breaks = c("2019.Mid-summer","2019.Late-summer","2020.Early-summer","2020.Mid-summer")) +
  labs(x = "Stream",
       y = "Genera Richness", 
       fill = "Time Period") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))




# Remove any groups to avoid grouping issues
Diversity_metrics_all_rich <- Diversity_metrics_all_rich %>% ungroup()

# Add missing combinations of Stream x year.season
Diversity_metrics_all_rich <- Diversity_metrics_all_rich %>%
  complete(Stream, Year.Season, fill = list(Shannon = 0))


# Add missing combos
Diversity_metrics_all_rich <- Diversity_metrics_all_rich %>%
  complete(Stream, Year.Season, fill = list(Shannon = 0))


ggplot(Diversity_metrics_all_rich, aes(x = Stream, y = Shannon, fill = Year.Season)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8
  ) +
  scale_fill_manual(
    values = c("yellowgreen", "indianred","cornflowerblue", "forestgreen"),
    breaks = c("2019.Mid-summer","2019.Late-summer","2020.Early-summer","2020.Mid-summer")
  ) +
  labs(x = "Stream",
       y = "Shannon", 
       fill = "Time Period") +
  theme_minimal() +                       # start with minimal theme
  theme(
    panel.background = element_rect(fill = "white"),  # white background
    panel.grid = element_blank(),                     # remove all grid lines
    axis.text.x = element_text(angle = 45, hjust = 1) # rotate x labels
  )

ggplot(Diversity_metrics_all_rich, aes(x = Stream, y = Shannon, fill = Year.Season)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9), width = 0.8) +
  scale_fill_manual(
    values = c("goldenrod1", "firebrick2","springgreen4", "royalblue3"),
    breaks = c("2019.Mid-summer","2019.Late-summer","2020.Early-summer","2020.Mid-summer")) +
  labs(x = "Stream",
       y = "Genera Shannon", 
       fill = "Time Period") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))









#Merging diversity metrics and standing stocks
chla_AFDM_data <-left_join(data_summary_by_Stream_period_genus, chla_AFDM_data, by= c('Stream','year','time period')) 
chla_afdm_diversity<-left_join(Diversity_metrics, chla_AFDM_data, by= c('Stream', 'year', 'time period')) #merging with diversity metrics

chla_afdm_diversity$Year.Season<- paste(chla_afdm_diversity$year, chla_afdm_diversity$`time period`, sep=".")  # again, creating new column to combine variables. 

#Merging diversity metrics and standing stocks
chla_AFDM_data <-left_join(data_summary_by_Stream_period_genus, chla_AFDM_data, by= c('Stream','year','time period')) 
chla_afdm_diversity<-left_join(Diversity_metrics, chla_AFDM_data, by= c('Stream', 'year', 'time period')) #merging with diversity metrics

chla_afdm_diversity$Year.Season<- paste(chla_afdm_diversity$year, chla_afdm_diversity$`time period`, sep=".")  # again, creating new column to combine variables. 





#getting means and merging data
#chla mean + sd
mean_chla <- chla_AFDM_data %>%
  group_by(Stream) %>%
  summarise(Mean_chla_mgperm2 = mean(`chla mgperm2`), SD_chla_mgperm2 = sd(`chla mgperm2`))
#mean_chla_data<-left_join(mean_chla_data, mean_chla, by= c('Stream'))
#afdm mean + sd
mean_afdm <- chla_AFDM_data %>%
  group_by(Stream) %>%
  summarise(Mean_afdmmgperm2 = mean(afdmmgperm2, na.rm = TRUE))

mean_chla_afdm <- left_join(mean_chla, mean_afdm, by = c('Stream'))
#richness mean + sd
mean_richness <- chla_AFDM_data %>%
  group_by(Stream) %>%
  summarise(Mean_Richness = mean(Genus.Richness.x), SD_Richness = sd(Genus.Richness.x))

mean_shannon <- chla_afdm_diversity %>%
  group_by(Stream) %>%
  summarise(Mean_Shannon = mean(Shannon), SD_Shannon = sd(Shannon))

mean_div_chla_afdm<-left_join(chla_afdm_diversity, mean_chla, by= c('Stream'))
mean_div_chla_afdm<-left_join(mean_richness, mean_shannon, by= c('Stream'))
mean_sd_all<-left_join(mean_div_chla_afdm, mean_chla, by= c('Stream'))
final_means_sd<-left_join(chla_AFDM_data, mean_sd_all, by= c('Stream'))

mean_sd_all <- mean_sd_all [-c(4), ]

#Mean species richness and mean chla
ggplot(mean_sd_all, aes(x = Mean_Richness, y = Mean_chla_mgperm2, color = Stream)) +
  geom_point(size = 3) +  # Mean values as scatter points
  geom_errorbar(aes(ymin = Mean_chla_mgperm2 - SD_chla_mgperm2, ymax = Mean_chla_mgperm2 + SD_chla_mgperm2), width = 0.2) +  # Y error bars
  geom_errorbarh(aes(xmin = Mean_Richness - SD_Richness, xmax = Mean_Richness + SD_Richness), height = 0.2) +  # X error bars
  theme_minimal() +  
  labs(x = "Mean Richness",
       y = "Mean Chlorophyll-a (mg/m²)",
       color = "Stream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

ggplot(mean_sd_all, aes(x = Mean_Richness, y = Mean_chla_mgperm2, color = Stream)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_chla_mgperm2 - SD_chla_mgperm2,
                    ymax = Mean_chla_mgperm2 + SD_chla_mgperm2), width = 0.2) +
  geom_errorbarh(aes(xmin = Mean_Richness - SD_Richness,
                     xmax = Mean_Richness + SD_Richness), height = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +   # no grey band
  theme_minimal() +
  labs(x = "Mean Richness",
       y = "Mean Chlorophyll-a (mg/m²)",
       color = "Stream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Mean shannon and mean chla
ggplot(mean_sd_all, aes(x = Mean_Shannon, y = Mean_chla_mgperm2, color = Stream)) +
  geom_point(size = 3) +  # Mean values as scatter points
  geom_errorbar(aes(ymin = Mean_chla_mgperm2 - SD_chla_mgperm2, ymax = Mean_chla_mgperm2 + SD_chla_mgperm2), width = 0.2) +  # Y error bars
  geom_errorbarh(aes(xmin = Mean_Shannon - SD_Shannon, xmax = Mean_Shannon + SD_Shannon), height = 0.2) +  # X error bars
  theme_minimal() +  
  labs(title = "Shannon Diversity and Chl-a",
       x = "Mean Shannon Diversity",
       y = "Mean Chlorophyll-a (mg/m²)",
       color = "Stream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

#richness and chla by time   


ggplot(chla_afdm_diversity, aes(x = Genus.Richness.x, 
                                y = `chla mgperm2`, 
                                color = Year.Season)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  scale_color_manual(
    values = c("yellow", "red", "green","blue"),
    breaks = c("2019.Mid-summer", "2019.Late-summer", "2020.Early-summer", "2020.Mid-summer")
  ) +
  labs(x = "Genera Richness",
       y = "Chlorophyll-a (mg/m²)",
       color = "Time Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#richness by stream 

ggplot(chla_afdm_diversity, aes(x = Genus.Richness.x, 
                                y = `chla mgperm2`, 
                                color = Stream)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Genera Richness",
       y = "Chlorophyll-a (mg/m²)",
       color = "Stream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


(ggplot(chla_afdm_diversity, aes(x = Sp.Richness, y = `chla mgperm2` , color = Stream)))+ #instead of stream could be Time period. or color could be time period.  
  geom_point()+geom_smooth(method='lm', se = FALSE)+ theme_bw()+
  #stat_cor(method = "pearson", aes(label = paste(..rr.label.., sep = "~`,`~")))+
  #stat_cor(method = "pearson", aes(label = paste(..p.label.., sep = "~`,`~")))+
  labs(x = "Species Richness",
       y = "Chlorophyll-a (mg/m²)",
       color = "Stream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
#GPP
chla_afdm_diversity_wo_JSP_LoGeo <- chla_afdm_diversity [-c(13,14), ] #removing JSP and LoGeo
GPP_chla_afdm_diversity_wo_JSP_LoGeo<-left_join(chla_afdm_diversity_wo_JSP_LoGeo, GPP_data, by= c('Stream', 'year', 'time period')) 

#GPP and richness by stream
(ggplot(GPP_chla_afdm_diversity_wo_JSP_LoGeo, aes(x = Sp.Richness, y = `mean gpp +/-3` , color = Stream)))+ #instead of stream could be Time period. or color could be time period.  
  geom_point()+geom_smooth(method='lm', se = FALSE)+ theme_bw()+
  #stat_cor(method = "pearson", aes(label = paste(..rr.label.., sep = "~`,`~")))+
  #stat_cor(method = "pearson", aes(label = paste(..p.label.., sep = "~`,`~")))+
  labs(x = "Species Richness",
       y = "mean gpp +/-3",
       color = "Stream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

#GPP and richness by time
(ggplot(GPP_chla_afdm_diversity_wo_JSP_LoGeo, aes(x = Sp.Richness, y = `mean gpp +/-3` , color = Year.Season.y)))+ #instead of stream could be Time period. or color could be time period.  
  geom_point()+geom_smooth(method='lm', se = FALSE)+ theme_bw()+ scale_color_manual(values =c("indianred","yellowgreen","forestgreen","cornflowerblue"))+
  #stat_cor(method = "pearson", aes(label = paste(..rr.label.., sep = "~`,`~")))+
  #stat_cor(method = "pearson", aes(label = paste(..p.label.., sep = "~`,`~")))+
  labs(x = "Species Richness",
       y = "mean gpp +/-3",
       color = "Time Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

mean_GPP <- GPP_chla_afdm_diversity_wo_JSP_LoGeo %>%
  group_by(Stream) %>%
  summarise(mean_GPP3 = mean(`mean gpp +/-3`), SD_MeanGPP = sd(`mean gpp +/-3`))
mean_sd_all <- left_join(mean_sd_all, mean_GPP, by= c('Stream'))

ggplot(mean_sd_all, aes(x = Mean_Richness, y = mean_GPP3, color = Stream)) +
  geom_point(size = 3) +  # Mean values as scatter points
  geom_errorbar(aes(ymin = mean_GPP3 - SD_MeanGPP, ymax = mean_GPP3 + SD_MeanGPP), width = 0.2) +  # Y error bars
  geom_errorbarh(aes(xmin = Mean_Richness - SD_Richness, xmax = Mean_Richness + SD_Richness), height = 0.2) +  # X error bars
  theme_minimal() +  
  labs(x = "Mean Richness",
       y = "mean gpp +/-3",
       color = "Stream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

#merge a data frame with time period so there's a random effect in the means data frame
library(lme4)
library(nlme)
GPP_chla_afdm_diversity_final <- GPP_chla_afdm_diversity_wo_JSP_LoGeo [-c(21:25, 28), ]

# idk if this has jasper in it or not.  Just include Jasper. Get rid of lower geode. 
GPP_chla_afdm_diversity_final$Year.Season.x <- as.factor(GPP_chla_afdm_diversity_final$`Year.Season.x`)
colnames(GPP_chla_afdm_diversity_final)<- c("Stream", "year","TimePeriod", "Sum_P_lnP", "Sum_simp"    ,         "Total"    ,           
                                                    "Shannon"           ,   "Simpsons"         ,    "Year.Season.x"      ,  "Stream.Year.Season.x", "Vegetation.x"   ,      "Sp.Richness"  ,       
                                                    "Sp.TotalNum"       ,   "TotalNum.Heterocysts" ,"chlamgperm2"      ,   "afdmgperm2"     ,      "afdmmgperm2"    ,      "Stream.Year.Season.y",
                                                   "Year.Season.y"   ,     "date"           ,      "Vegetation.y"    ,     "meangpp_3"     ,   "meangpp_7"   )


print(colnames(GPP_chla_afdm_diversity_final))

#Chla and richness
model <- lme(`Shannon` ~ Vegetation, 
             random = ~1 | `Year.Season`, 
             data = Diversity_metrics_all_rich)
summary(model)
plot(model)
anova(model)

#chla and shannon
model1 <- lme(`Sp.Richness` ~ Vegetation, 
             random = ~1 | `Year.Season`, 
             data = Diversity_metrics_all_rich)
summary(model1)
plot(model1)
anova(model1)

colnames(chla_AFDM_data) [10] <- "chlamgperm2"
#chla and shannon
model1 <- lme(`chlamgperm2` ~ Vegetation, 
              random = ~1 | `Year.Season`, 
              data = chla_AFDM_data)
summary(model1)
plot(model1)
anova(model1)

#chla and shannon
model1 <- lme(`afdmmgperm2` ~ Vegetation, 
              random = ~1 | `Year.Season`, 
              data = chla_AFDM_data, 
              na.action = na.omit)
summary(model1)
plot(model1)
anova(model1)

#afdm and Shanon  
model3 <- lme(afdmmgperm2 ~ Shannon, 
              random = ~1 | `Year.Season.x`, 
              data = GPP_chla_afdm_diversity_final, 
              na.action = na.omit)
summary(model3)
plot(model3)
anova(model3)

#Mean Richness and Mean Chla
mean_all<-left_join(mean_div_chla_afdm, mean_chla_afdm, by= c('Stream'))
colrows(mean_all)
mean_all <- mean_all[-4, ]

#model3 <- lme(`Mean_chla_mgperm2` ~ Mean_Richness, 
              #random = ~1 | `Year.Season.x`, 
              #data = GPP_chla_afdm_diversity_final, 
             # na.action = na.omit)
#summary(model3)
#plot(model3)
#anova(model3)


#GPP and chla 
model4 <- lme(meangpp_3 ~ chlamgperm2 , 
              random = ~1 | `Year.Season.x`, 
              data = GPP_chla_afdm_diversity_final, 
              na.action = na.omit)
summary(model4)
plot(model4)
anova(model4)

#GPP and afdm
model5 <- lme(meangpp_3 ~ afdmmgperm2 , 
              random = ~1 | `Year.Season.x`, 
              data = GPP_chla_afdm_diversity_final, 
              na.action = na.omit)
summary(model5)
plot(model5)
anova(model5)

#GPP and richnes
model6 <- lme(meangpp_3 ~ Sp.Richness , 
              random = ~1 | `Year.Season.x`, 
              data = GPP_chla_afdm_diversity_final, 
              na.action = na.omit)
summary(model6)
plot(model6)
anova(model6)

#GPP and shannon
model7 <- lme(meangpp_3 ~ Shannon , 
              random = ~1 | `Year.Season.x`, 
              data = GPP_chla_afdm_diversity_final, 
              na.action = na.omit)
summary(model7)
plot(model7)
anova(model7)

df_long_species <- new_data_raw_species [,-c(4,5,6,7,9,10,11,12,14, 16:25)]             

df_wide_species <- df_long_species %>%  
  pivot_wider(names_from = "Species", values_from = "No.Individuals", values_fill = 0,  values_fn = sum )%>% ungroup()
  df_wide_species$year<-as.character(df_wide_species$year) # converting year to character and not numeric
df_wide_species <- df_wide_species [-c(16), ]

colnames (df_wide_species) [2] <- "time.period"
colnames (df_wide_species)
colnames (new_data_raw_species)


#write.csv(df_wide_species, "df_wide_species.csv")

#BETA DIVERSITY STUFF
install.packages("betapart") #only have to do this once. 
library(betapart)

df_wide_species$year<-as.character(df_wide_species$year) # converting year to character and not numeric

Species_binary <- df_wide_species %>% # creates presence absence 
  mutate(across(where(is.numeric), ~ ifelse(. > 0, 1, 0)))
#write_csv(Genus_binary, "Genus.binary.csv")

#not step 1 anymore- step 3ish) filter data frame (either data_raw_species or data_raw_genus depending on what we decide to work with) to include only:
#   data from one time period 

#Comparison of 2019 mid to late summer
new_raw_df_2019_T1<- filter(Species_binary , year =="2019", `time.period` == "Mid-summer")
new_raw_df_2019_T2<- filter(Species_binary , year =="2019", `time.period` == "Late-summer")

new_raw_df_2019_T1<- select(new_raw_df_2019_T1, -c("Stream":"Vegetation"))
new_raw_df_2019_T1<- select(new_raw_df_2019_T1, -c("Stream.Year":"Year.Season"))
new_raw_df_2019_T2<- select(new_raw_df_2019_T2, -c("Stream":"Vegetation"))
new_raw_df_2019_T2<- select(new_raw_df_2019_T2, -c("Stream.Year":"Year.Season"))

Comparison_2019<- beta.temp(new_raw_df_2019_T1, new_raw_df_2019_T2, index.family="sorensen") #.sne = nestedness, .sim= turnover, .sor= total dissimilarity
Comparison_2019$Stream<- c("Ant","EFBT","Geode","Lost","Rose")
colnames(Comparison_2019)[1]<- "turnover"
colnames(Comparison_2019)[2]<- "nestedness"
colnames(Comparison_2019)[3]<- "total dissimilarity"
#write.csv(Comparison_2019, "BetaDiversity_2019.csv")


#Comparison of 2020 early to mid summer
df_2020_E<- filter(Species_binary , year =="2020", `time.period` == "Early-summer")
df_2020_M<- filter(Species_binary , year =="2020", `time.period` == "Mid-summer")

df_2020_E<- select(df_2020_E, -c("Stream":"Year.Season"))
df_2020_M<- select(df_2020_M, -c("Stream":"Year.Season"))

Comparison_2020<- beta.temp(df_2020_E, df_2020_M, index.family="sorensen") #.sne = nestedness, .sim= turnover, .sor= total dissimilarity
Comparison_2020$Stream<- c("Ant","EFBT","Geode","Lost","Rose")
colnames(Comparison_2020)[1]<- "turnover"
colnames(Comparison_2020)[2]<- "nestedness"
colnames(Comparison_2020)[3]<- "total dissimilarity"
#write.csv(Comparison_2020, "BetaDiversity_2020.csv")


#Comparison of 2019 to 2020 mid summer to mid summer
df_2019_Mid<- filter(Species_binary , year =="2019", `time.period` == "Mid-summer")
df_2020_Mid<- filter(Species_binary , year =="2020", `time.period` == "Mid-summer")

df_2019_Mid<- select(df_2019_Mid, -c("Stream":"Year.Season"))
df_2020_Mid<- select(df_2020_Mid, -c("Stream":"Year.Season"))

Comparison_Mid<- beta.temp(df_2019_Mid, df_2020_Mid, index.family="sorensen") #.sne = nestedness, .sim= turnover, .sor= total dissimilarity
Comparison_Mid$Stream<- c("Ant","EFBT","Geode","Lost","Rose")
colnames(Comparison_Mid)[1]<- "turnover"
colnames(Comparison_Mid)[2]<- "nestedness"
colnames(Comparison_Mid)[3]<- "total dissimilarity"
#write.csv(Comparison_Mid, "BetaDiversity_2019_2020.csv")
Comparison_2019$TimePeriod<- "2019_Mid-Late"
Comparison_2020$TimePeriod<- "2020_Early-Mid"
Comparison_Mid$TimePeriod<- "Mid-Mid"

Comparison_all<- rbind(Comparison_2019, Comparison_2020, Comparison_Mid)

Comparison_all$TimePeriod<- factor(Comparison_all$TimePeriod, levels = c("2020_Early-Mid", "Mid-Mid", "2019_Mid-Late")) # do this to reorder for figure
Comparison_all <- Comparison_all[order(Comparison_all$TimePeriod), ]

#BETA DIVERSITY STUFF
long_beta_test <- Comparison_all %>%
  pivot_longer(cols = c(turnover, nestedness), names_to = "Component", values_to = "Value") #combined nestedness and turnover into one column
mid_mid_data <- filter(long_beta_test, grepl("Mid-Mid", TimePeriod, ignore.case = TRUE)) #added the mid-mid to its own data frame
long_beta <- filter(long_beta_test, !grepl("Mid-Mid", TimePeriod, ignore.case = TRUE)) #removed mid-mid from the orginal
long_beta$Stream.TimePeriod<- paste(long_beta$Stream, long_beta$TimePeriod, sep=".")  # again, creating new column to combine variables. 
#long_beta$Stream.TimePeriod <- factor(long_beta$Stream.TimePeriod, levels = c(""))
#data_genus_chla_afdm <- data_genus_chla_afdm[order(data_genus_chla_afdm$Year.Season), ]

#changing the name of Stream and Time Period
new_long_beta <- long_beta %>%
  mutate(Stream.TimePeriod = recode(Stream.TimePeriod, 
                                    "Ant.2019_Mid-Late" = "Ant 2019 Mid-Late",
                                    "Ant.2020_Early-Mid" = "Ant 2020 Early-Mid",
                                    "EFBT.2019_Mid-Late" = "EFBT 2019 Mid-Late", 
                                    "EFBT.2020_Early-Mid" = "EFBT 2020 Early-Mid", 
                                    "Geode.2019_Mid-Late" = "Geode 2019 Mid-Late", 
                                    "Geode.2020_Early-Mid" = "Geode 2020 Early-Mid", 
                                    "Lost.2019_Mid-Late" = "Lost 2019 Mid-Late",
                                    "Lost.2020_Early-Mid" = "Lost 2020 Early-Mid", 
                                    "Rose.2019_Mid-Late" = "Rose 2019 Mid-Late", 
                                    "Rose.2020_Early-Mid" = "Rose 2020 Early-Mid"))
new_long_beta$Stream.TimePeriod <- factor(new_long_beta$Stream.TimePeriod, levels = c("Ant 2020 Early-Mid", "Ant 2019 Mid-Late", 
                                                                                      "EFBT 2020 Early-Mid", "EFBT 2019 Mid-Late",
                                                                                      "Geode 2020 Early-Mid","Geode 2019 Mid-Late", 
                                                                                      "Lost 2020 Early-Mid", "Lost 2019 Mid-Late", 
                                                                                      "Rose 2020 Early-Mid", "Rose 2019 Mid-Late"))
new_long_beta <- new_long_beta[order(new_long_beta$Stream.TimePeriod), ]
new_long_beta$`total dissimilarity` <- NULL

ggplot(new_long_beta, aes(x = Stream.TimePeriod, y = Value, fill = Component, color = TimePeriod)) +
  scale_fill_manual(values = c("white", "gray50"))+
  geom_bar(stat = "identity", size = 1.2)+
  labs(x = "Stream", y = "Beta Diversity", fill = "Component") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  scale_x_discrete(labels = c("Ant 2019 Mid-Late" = "Ant, \nMid-Late", "Ant 2020 Early-Mid" = "Ant, \nEarly-Mid",
                              "EFBT 2019 Mid-Late" = "EFBT,\nMid-Late", "EFBT 2020 Early-Mid" = "EFBT, \nEarly-Mid", 
                              "Geode 2019 Mid-Late" = "Geode, \nMid-Late", "Geode 2020 Early-Mid" = "Geode, \nEarly-Mid", 
                              "Lost 2019 Mid-Late" = "Lost, \nMid-Late", "Lost 2020 Early-Mid" = "Lost, \nEarly-Mid", 
                              "Rose 2019 Mid-Late" = "Rose, \nMid-Late", "Rose 2020 Early-Mid" = "Rose,\nEarly-Mid"))

ggplot(mid_mid_data, aes(x = Stream, y = Value, fill = Component, color = TimePeriod)) +  # Fix: Closing parenthesis
  scale_fill_manual(values = c("white", "gray50")) +
  scale_color_manual(values = c("chartreuse4")) +  # Added color scale
  geom_bar(stat = "identity", size = 1.5) +
  labs(x = "Stream", y = "Beta Diversity", fill = "Component") +
  theme_minimal()

