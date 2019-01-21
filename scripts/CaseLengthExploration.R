# Case Length Exploration
  # Damini Sharma
  # January 20, 2019

'In this file, I create a few different graphs looking at the length of cases 
that go through the Cook County State Attorneys Office. First, I explore the 
sentences dataset to look at average length of cases in days from 2011 to 2018, 
plotted alongside the total number of defandants receiving sentences in 
that time period. Next, I look at the number of cases coming into the SAO using the
Intake and Initiations data, and see if there are any particular offense types 
that are driving the overall trends in number of cases sentenced. Finally, I look 
at the distribution of case lengths by offense types before and after 2016, which
is when we see a significant drop in the number of cases'

# Read in the data
intake <- read.csv(here("data_source","SAO Data","Intake.csv"),stringsAsFactors = FALSE) 
initiation <- read.csv(here("data_source","SAO Data","Initiation.csv"),stringsAsFactors = FALSE) 
sentence <- read.csv(here("data_source","SAO Data","Sentencing.csv"),stringsAsFactors = FALSE) 
disposition <- read.csv(here("data_source","SAO Data","Dispositions.csv"),stringsAsFactors = FALSE) 


# For graph 1, we look at sentences data only. 
# There is already a length of case in days variable but this is from arraignment to 
# sentencing, and I want to start at arrest so I define my own

# Since the sentencing data is not unique on case participant ID, I first collapse the data
# to get an average case length per participant ID. This is because many defendants will have
# multiple charges, and say each charge takes 8 days to go from arraignment to sentencing. Those
# are the same 8 days, so we don't want to double count them.
sentence %<>%
  mutate(SENTENCE_DATE = as.Date(SENTENCE_DATE,'%m/%d/%Y' )) %>% 
  mutate(ARREST_DATE = as.Date(ARREST_DATE,'%m/%d/%Y' )) %>% 
  mutate(case_length_total = unclass(difftime(SENTENCE_DATE,ARREST_DATE,units = "days"))) %>%
  mutate(sentence_month = round_date(SENTENCE_DATE, "month"))  

case_length <- sentence %>% 
  group_by(sentence_month, CASE_PARTICIPANT_ID) %>% 
  summarise(case_length_p = mean(case_length_total, na.rm = TRUE)) %>% 
  group_by(sentence_month) %>% 
  summarise(total_felonies = n_distinct(CASE_PARTICIPANT_ID), 
            total_case_length = sum(case_length_p, na.rm = TRUE)/1000,
            avg_case_length = mean(case_length_p, na.rm = TRUE)) %>% 
  filter(total_felonies > 0) %>% 
  filter(total_case_length > 0) %>% 
  filter(avg_case_length > 0 & avg_case_length < 2000) %>% 
  filter(sentence_month > "2011-01-01" & sentence_month < "2018-01-01") %>% 
  gather(type_measure,n,total_felonies:avg_case_length)


plot1 <- case_length %>% 
  filter(type_measure %in% c("total_felonies","avg_case_length")) %>% 
  ggplot(aes(x=sentence_month, y=n, linetype=type_measure)) + 
  geom_line(data=case_length %>% filter(type_measure=="total_felonies"),
            size = .5, alpha=0.8,color="brown3") + 
  geom_line(data=case_length %>% filter(type_measure=="avg_case_length"),
            aes(y = n*6),size=0.6,color="brown4") +
  #geom_line(data=case_length %>% filter(type_measure == "total_case_length"),
  #          aes(y=n*6), alpha=0.6) +
  scale_y_continuous(sec.axis = sec_axis(~ ./6, name = "Length of Case (Days)")) +
  #scale_color_manual(name="", values=c("brown4","blue4","green")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    subtitle = "Cook County Crimes Sentenced From 2011 to 2018",
    title = "Average case length increases even as total defendants sentenced decrease",
    y = "Number of Defendants",
    x = "Year",
    caption = "Source: Cook County State Attorney's Office Data Portal: Sentences"
  ) +
  geom_text(aes(x = as.Date("2017-01-01"), y = 2400, label = "Average Case Length"),color="brown") +
  geom_text(aes(x = as.Date("2017-01-01"), y = 1400, label = "Total Defendants Sentenced"),color="brown") 

ggsave(here("data_output","cases_sentenced_avg_case_length.pdf"),plot1)

# Next, we construct the graphs to figure out if if any particular types of
# felonies are causing the increase or decrease in total defendants sentenced

# For this, need to merge intake, initiation, and sentencing data, clean offenses category,
# and construct variables for part of the process

intake %<>% 
  mutate(RECEIVED_DATE = as.Date(RECEIVED_DATE,'%m/%d/%Y')) %>% 
  mutate(receive_month = round_date(RECEIVED_DATE, "month")) %>% 
  mutate(Offense_Category_broad = case_when(
    grepl("Retail Theft", Offense_Category) ~ "Retail Theft",
    grepl("Burglary", Offense_Category) ~ "Burglary",
    grepl("Homicide", Offense_Category) ~ "Homicide",
    grepl("Robbery", Offense_Category) ~ "Robbery",
    grepl("Battery", Offense_Category) ~ "Battery",
    grepl("DUI", Offense_Category) ~ "DUI",
    grepl("UUW", Offense_Category) ~ "Unlawful Use of Weapon",
    Offense_Category == "Theft" ~ "Theft",
    Offense_Category == "Narcotics" ~ "Narcotics",
    Offense_Category == "Sex Crimes" ~ "Sex Crimes",
    Offense_Category == "Possession of Stolen Motor Vehicle" ~ "MVT",
    Offense_Category == "Driving With Suspended Or Revoked License" ~ "Driving With Revoked License",
    TRUE ~ ""    
  )) %>% 
  filter(Offense_Category_broad != "") %>% 
  filter(receive_month > "2011-01-01" & receive_month < "2018-01-01") 


intake_initiation <- inner_join(intake,initiation, by = "CASE_PARTICIPANT_ID")

intake_initiation_fel_by_offense <- intake_initiation %>% 
  group_by(receive_month, Offense_Category_broad) %>% 
  summarise(felonies = n_distinct(CASE_PARTICIPANT_ID))

plot2 <- ggplot(intake_initiation_fel_by_offense, aes(y=felonies, x=receive_month, color=Offense_Category_broad))  +
  geom_point(size=0.2) +
  geom_smooth(size=0.1) +
  facet_wrap(~Offense_Category_broad, scales = "free") +
  scale_color_manual(values=c("chocolate3","chocolate3","chocolate3", "blue", 
                              "chocolate3", "chocolate3", "blue", "blue",
                              "chocolate3", "chocolate3","chocolate3","chocolate3")) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(strip.text = element_text(size = rel(0.7))) +
  labs(
    title = "DUIs, narcotics, and retail theft cases drive decrease in number of cases",
    subtitle = "Number of cases by offense type from 2011 to 2018",
    caption = "Source: Cook County State Attorney's Office Data Portal: Initiations and Intake",
    x = "Year",
    y = "Number of Cases"
  )


ggsave(here("data_output","num_cases_by_offense.pdf"),plot2)


# Next, want to merge intake/initiation data with sentences data to look at the
# distribution of case length by offense type, pre 2016 and post 2016

int_ini_sent <- inner_join(intake_initiation,sentence,by="CASE_PARTICIPANT_ID") %>% 
  filter(sentence_month > "2011-01-01" & sentence_month < "2018-01-01")

# Want to calculate avg length per case (first collapse by Case participant) and then avg length
# per offense type, broken down into 2 time periods

lengths_by_offense <- int_ini_sent %>% 
  group_by(sentence_month, Offense_Category_broad, CASE_PARTICIPANT_ID) %>% 
  summarise(case_length_p = mean(case_length_total, na.rm = TRUE)) %>% 
  group_by(sentence_month, Offense_Category_broad) %>% 
  summarise(avg_case_length = mean(case_length_p, na.rm = TRUE)) %>% 
  filter(avg_case_length > 0 & avg_case_length < 2000) %>% 
  filter(sentence_month > "2011-01-01" & sentence_month < "2018-01-01")

lengths_by_offense <- int_ini_sent %>% 
  mutate(after_2016 = ifelse(sentence_month > "2016-01-01",1,0)) %>% 
  group_by(after_2016, Offense_Category_broad, CASE_PARTICIPANT_ID) %>% 
  summarise(case_length_p = mean(case_length_total, na.rm = TRUE)) %>% 
  filter(case_length_p > 0 & case_length_p < 2000) %>% 
  filter(Offense_Category_broad %in% c("DUI","Narcotics","Homicide","Retail Theft",
                                       "Driving With Revoked License",
                                       "Unlawful Use of Weapon", "Burglary","Sex Crimes")) %>% 
  mutate(off_cat_rename = case_when(
    Offense_Category_broad == "Retail Theft" ~ "2. Retail Theft",
    Offense_Category_broad == "Narcotics" ~ "7. Narcotics",
    Offense_Category_broad == "DUI" ~ "6. DUI",
    Offense_Category_broad == "Unlawful Use of Weapon" ~ "4. Unlawful Use of Weapon",
    Offense_Category_broad == "Burglary" ~ "3. Burglary",
    Offense_Category_broad == "Driving With Revoked License" ~ "5. Driving With Revoked License",
    Offense_Category_broad == "Homicide" ~ "1. Homicide",
    Offense_Category_broad == "Sex Crimes" ~ "8. Sex Crimes"
  ))


lengths_by_offense$after_2016 <- factor(lengths_by_offense$after_2016, labels = c("Before 2016", "After 2016"))

plot3 <- ggplot(lengths_by_offense) + 
  geom_density(aes(x=case_length_p,color=off_cat_rename),alpha = 0.3,show.legend = FALSE) +
  stat_density(aes(x=case_length_p,color=off_cat_rename),geom="line",position = "identity") +
  facet_wrap(~after_2016) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(
    title = "Average case lengths increasing across offense types, even as retail theft and narcotics cases decrease",
    subtitle = "Case lengths by offense type from 2011 to 2018",
    caption = "Source: Cook County State Attorney's Office Data Portal: Initiations, Intake, and Sentences",
    x = "Length of Case in Days",
    y = "Density",
    color = "Offense Type"
  )

ggsave(here("data_output","case_length_offense_type.pdf"),plot3)