### Load libraries
library(tidyverse)#For data wrangling and plotting


### Read data
admissions_data<- read.csv("data/data_admissions_csv.csv")

### Checking for NA in the relevant columns
sum(is.na(admissions_data$"sex"),is.na(admissions_data$"condition_adjusted"))
# NA is present so must be filtered out

###Preparing the data
#We want to calculate the percentage of outcomes for each sex
percent_outcome <- admissions_data %>% 
  filter(!is.na(sex))%>% #Filtering NA
  filter(!is.na(condition_adjusted))%>%
  filter(diagnosis!="ikke sindssyg") %>% #Filtering "ikke sindssyg" ("not insane") as this is not a diagnosis
  count(sex,condition_adjusted) %>%
  group_by(sex) %>% 
  mutate(percent=round((n/sum(n))*100,digits = 0)) %>% 
  ungroup()

#Preparing facet names
facet_labels <- c(
  'bedret'="Bettered",
  'doed'="Died",
  'genindlagt'="Re-admitted",
  'helbredt'="Cured",
  'uhelbredt'="Uncured"
)


###Plotting
fig_outcome_by_sex <- percent_outcome %>%
  ggplot(aes(x=sex,y=percent,fill = sex))+#adding color
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=paste0(percent,"%")),position = position_stack(vjust=1))+
  facet_grid(~condition_adjusted, labeller=as_labeller(facet_labels))+  
  theme(panel.grid=element_blank(),
        legend.position = "none",#Removing unnecessary legend and left aligning caption
        plot.caption = element_text(hjust=-0.1),
        axis.text.x =element_text(angle=45,hjust = 1,vjust = 1))+
  scale_x_discrete(breaks=c("f","m"),
                   labels=c("Women","Men"))+
  labs(title = "Outcome by sex at Jutland Asylum",
       subtitle = "Of those admitted 1890-1891",
       tag = "Figure 3.1",
       fill="Outcome",
       x="Sex",
       y="Percent by sex", #Renaming lables
       caption ="Sources: \n Psykiatrisk Hospital Risskov protokol over optagne patienter,mænd 1889 - 1913 \n Psykiatrisk Hospital Risskov protokol over optagne patienter, kvinder 1889 - 1913")

fig_outcome_by_sex

###Saving the plot
ggsave("output/fig3.1_outcome_by_sex.png",fig_outcome_by_sex,height=6,width = 6)
