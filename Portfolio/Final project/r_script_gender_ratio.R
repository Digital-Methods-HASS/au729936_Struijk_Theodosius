### Load libraries
library(tidyverse)#For data wrangling and plotting


### Read data
admissions_data<- read.csv("data/data_admissions_csv.csv")

### Checking for NA in the relevant columns
sum(is.na(admissions_data$"sex"))
# No NA

###Calculating percentage
gender_percent <- admissions_data %>% 
  group_by(sex) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=round((n/sum(n))*100,digits=0))

###Plotting
fig_gender_ratio <- gender_percent %>%
  ggplot(aes(x="",y=percent,fill = sex))+#adding color
  geom_bar(stat = "identity",width=1)+
  coord_polar("y",start=0)+
  geom_text(aes(label=paste0(percent,"%","\n","Admittances:",n)),position=position_stack(vjust=0.5))+
  theme_void()+
  theme(plot.caption = element_text(hjust=-0.1))+#left aligning caption
  scale_fill_discrete(labels=c("Women","Men"))+
  labs(title ="Sex distribution of admissions to the Jutland Asylum \n 1890-1891",
       fill="",
       tag ="Figure 1",
       caption ="Sources: \n Psykiatrisk Hospital Risskov protokol over optagne patienter,mænd 1889 - 1913 \n Psykiatrisk Hospital Risskov protokol over optagne patienter, kvinder 1889 - 1913")

fig_gender_ratio

###Saving the plot
ggsave("output/fig1_gender_ratio.png",fig_gender_ratio,height=6,width = 6)
