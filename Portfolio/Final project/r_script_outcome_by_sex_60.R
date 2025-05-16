### Load libraries
library(tidyverse)#For data wrangling


### Read data
admissions_data<- read.csv("data/data_admissions_csv.csv")

### Checking for NA in the relevant columns
sum(is.na(admissions_data$"sex"),is.na(admissions_data$"condition_adjusted"))
# NA is present so must be filtered out



###Preparing the data
##First we need to calculate the age of the patients at their departure dates, 
#so we can filter those out who died at the age of 60 or older
admissions_date <- admissions_data %>%
  drop_na(admission_yyyy,admission_mm,admission_dd,departure_yyyy,departure_mm,departure_dd) %>% #Filtering na from date columns
  mutate(ad_date=as_date(paste(admission_yyyy,admission_mm,admission_dd,sep = "-"))) %>% #Mutating and reformatting date columns into ymd
  mutate(dep_date=as_date(paste(departure_yyyy,departure_mm,departure_dd,sep = "-"))) %>%
  mutate(dur=year(as.period(interval(ad_date, dep_date)))) %>% #Calculating the interval between arival dates and departure dates in years
  mutate(age_dep=age+dur)#calculating age ad departure

#Then we want to filter those over 60, NA's and "not insane" 
data_filtered <- admissions_date %>% 
  drop_na(sex,condition_adjusted) %>% 
  filter(diagnosis!="ikke sindssyg") %>% #Filtering "ikke sindssyg" ("not insane") as this is not a diagnosis
  filter(age_dep<60)

#Then we want to calculate the percentage of outcomes for each sex
percent_outcome_60 <- data_filtered %>% 
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
fig_outcome_by_sex_60 <- percent_outcome_60 %>%
  ggplot(aes(x=sex,y=percent,fill = sex))+
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=paste0(percent,"%")),position = position_stack(vjust=1))+#Adding percentage lables
  facet_grid(~condition_adjusted, labeller=as_labeller(facet_labels))+  
  theme(panel.grid=element_blank(),
        legend.position = "none",#Removing unnecessary legend
        plot.caption = element_text(hjust=-0.1),
        axis.text.x =element_text(angle=45,hjust = 1,vjust = 1))+
  scale_x_discrete(breaks=c("f","m"),
                   labels=c("Women","Men"))+ #Renaming lables
  labs(title = "Outcome by sex at Jutland Asylum",
       subtitle = "Of those und the age of 60 admitted 1890-1891",
       tag = "Figure 3.2",
       fill="Outcome",
       x="Sex",
       y="Percent by sex", 
       caption ="Sources: \n Psykiatrisk Hospital Risskov protokol over optagne patienter,mænd 1889 - 1913 \n Psykiatrisk Hospital Risskov protokol over optagne patienter, kvinder 1889 - 1913")

fig_outcome_by_sex_60

###Saving the plot
ggsave("output/fig3.2_outcome_by_sex_60.png",fig_outcome_by_sex_60,height=6,width = 6)  
