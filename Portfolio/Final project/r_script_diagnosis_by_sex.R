### Load libraries
library(tidyverse)#For data wrangling


### Read data
admissions_data<- read.csv("data/data_admissions_v_1_csv.csv")

### Checking for NA in the relevant columns
sum(is.na(admissions_data$"sex"),is.na(admissions_data$"diagnosis"))
# No NA is present; no need to remove them

### Checking unique diagnoses
unique(admissions_data$diagnosis_adjusted)
#"ikke sindssyg" ("not insane") is not a diagnosis. If we want to look at diagnoses only, we need to filter it out

###Calculating percentage
percent_diagnosis <- admissions_data %>% 
  filter(diagnosis_adjusted!="ikke sindssyg") %>% #Filtering "ikke sindssyg"
  count(sex,diagnosis_adjusted) %>%
  group_by(sex) %>% 
  mutate(percent=round((n/sum(n))*100,digits = 0)) %>% 
  ungroup()

#Preparing facet names
facet_labels <- c(
  'dementia'="Dementia",
  'fatuitet'="Fatuity",
  'mani'="Mania",
  'melankoli'="Melancholy",
  'vanvid'="Insanity"
)



###Plotting

fig_diagnoses_by_sex <- percent_diagnosis %>%
  ggplot(aes(x=sex,y=percent,fill = sex))+#adding color
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=paste0(percent,"%")),position = position_stack(vjust=1))+
  facet_grid(~diagnosis_adjusted, labeller=as_labeller(facet_labels))+
  theme_gray()+
  theme(panel.grid=element_blank(),
        legend.position = "none",#Removing unnecessary legend and left aligning caption
        plot.caption = element_text(hjust=-0.1),
        axis.text.x =element_text(angle=45,hjust = 1,vjust = 1))+
  scale_x_discrete(breaks=c("f","m"),
                   labels=c("Women","Men"))+
  labs(title = "Diagnoses by sex given at Jutland Asylum \n 1890-1891",
       fill="Diagnoses",
       tag = "Figure 2",
       x="Sex",
       y="Percent by sex",#Renaming labels,
       caption ="Sources: \n Psykiatrisk Hospital Risskov protokol over optagne patienter,mænd 1889 - 1913 \n Psykiatrisk Hospital Risskov protokol over optagne patienter, kvinder 1889 - 1913")

fig_diagnoses_by_sex

###Saving the plot
ggsave("output/fig2_diagnoses_by_sex.png",fig_diagnoses_by_sex,height=6,width = 6)       
