




# Read in the data
intake <- read.csv(here("data_source","SAO Data","Intake.csv")) 
initiation <- read.csv(here("data_source","SAO Data","Initiation.csv")) 
sentence <- read.csv(here("data_source","SAO Data","Sentencing.csv")) 
disposition <- read.csv(here("data_source","SAO Data","Dispositions.csv")) 

table(initiation$EVENT)









# Check number of observations in sentencing data for second chance probation
cbind(table(sentence$SENTENCE_TYPE))
summary(intake)
summary(initiation)
cbind(table(sentence$SENTENCE_TYPE))


# Keep participant IDs for people who got sentence type
sentence1 <- sentence %>% 
                filter(SENTENCE_TYPE=="2nd Chance Probation")

# Is this unique?
length(unique(sentence1$CASE_PARTICIPANT_ID))

# no, 1067 is unique of 1080 obs. need to dedup then merge back
sentence1 <- sentence1 %>% 
                select(CASE_ID,CASE_PARTICIPANT_ID,GENDER,RACE) %>% 
                distinct(CASE_ID,CASE_PARTICIPANT_ID,GENDER,RACE)

# now we have 1067 unique IDs. merging this back to universe of sentence data to get all their charges
sentence2 <- inner_join(sentence,sentence1,by="CASE_PARTICIPANT_ID")
sentence2 <- sentence2 %>% 
              select(CASE_PARTICIPANT_ID, PRIMARY_CHARGE)

# now we have 1082 obs. 1067 are unique so some people have multiple charges
# only 909 of these have primary charge == true which means that some people have their
# primary charges thrown out. this information should be in dispositions
# so it would be interesting to see which crime types are thrown out as primary charges
# in cases where a non primary charge results in second chance probation

  # table(sentence2$PRIMARY_CHARGE)
  # cbind(table(sentence2$RACE))
  # cbind(table(sentence1$GENDER))

  colnames(sentence2)

# no i want to limit to people who have a primary charge = true in my 1082 obs. i want to remove
# those people and look at the leftover. then merge those onto disposition
  sentence3 <- sentence2 %>% 
                filter(PRIMARY_CHARGE=="true") %>% 
                select(CASE_PARTICIPANT_ID)

  sentence_only_falses <- anti_join(sentence2,sentence3, by="CASE_PARTICIPANT_ID")   
  sentence_only_falses <- sentence_only_falses %>% 
                          select(CASE_PARTICIPANT_ID)
  
  sentence_primary_charges <- inner_join(disposition,sentence_only_falses,by="CASE_PARTICIPANT_ID")
  sentence_primary_charges <- sentence_primary_charges %>% 
                              arrange(CASE_PARTICIPANT_ID)
  
# now i have 849 observations
  cbind(table(sentence_primary_charges$CHARGE_DISPOSITION))
  cbind(table(sentence_primary_charges$CHARGE_DISPOSITION_REASON))
  
  table(sentence_primary_charges$PRIMARY_CHARGE)
  
  sentence_primary_charges_NG <- sentence_primary_charges %>% 
                                  filter(PRIMARY_CHARGE == "true") %>% 
                                  filter(CHARGE_DISPOSITION == "FNG" | CHARGE_DISPOSITION == "Nolle Prosecution" ) 

  sentence_primary_charges_NG <- sentence_primary_charges_NG %>% 
                                  mutate(OFFENSE_TYPE_char = as.character(OFFENSE_TYPE))
  
  cbind(table(sentence_primary_charges_NG$OFFENSE_TYPE_char))
  counts <- table(sentence_primary_charges_NG$OFFENSE_TYPE_char)
  counts <- as.data.frame(counts)
  counts <- counts %>% 
              arrange(Freq) %>% 
              mutate(nameOrder = factor(Var1,levels=Var1)) %>% 
              select(nameOrder, Freq)
  
  
  ggplot(counts,aes(nameOrder,Freq)) +
      geom_col() +
      coord_flip() +
      labs(title = "Dropped Primary Charges",
            subtitle= "Where Participant Case Received Second Chance Probation" ) +
      theme(axis.title.y = element_blank())
  
  cbind(table(sentence_primary_charges$CHARGE_DISPOSITION))
  sentence_primary_charges_G <- sentence_primary_charges %>% 
                      filter(CHARGE_DISPOSITION == "Finding Guilty" | CHARGE_DISPOSITION == "Plea Of Guilty" ) 
  
  
  cbind(table(sentence_primary_charges_G$OFFENSE_TYPE, sentence_primary_charges_G$CHARGE_DISPOSITION))
  sentence_primary_charges_G <- sentence_primary_charges_G %>% 
                      mutate(OFFENSE_TYPE_char = as.character(OFFENSE_TYPE))
  
  counts2 <- table(sentence_primary_charges_G$OFFENSE_TYPE_char)
  counts2 <- as.data.frame(counts2)
  counts2 <- counts2 %>% 
    arrange(Freq) %>% 
    mutate(nameOrder = factor(Var1,levels=Var1)) %>% 
    select(nameOrder, Freq)
  
  
  ggplot(counts2,aes(nameOrder,Freq)) +
    geom_col() +
    coord_flip() +
    labs(title = "Charges With Guilty Plea",
         subtitle= "Where Participant Case Received Second Chance Probation" ) +
    theme(axis.title.y = element_blank())
  
##############################################################################
  # Break down by Race 
  ###########################################################################
  
  
  # Keep sentences after 2nd change probation was enacted (January 1st, 2014)
  class(sentence$SENTENCE_DATE)
  table(sentence$SENTENCE_DATE)
  sentence$SENTENCE_DATE <- mdy_hms(sentence$SENTENCE_DATE)
  as.numeric(sentence$SENTENCE_DATE)
  
  sentence$CUTOFF_DATE <- mdy("January 1, 2014")
  sent_post2014 <- sentence[which(sentence$SENTENCE_DATE >= sentence$CUTOFF_DATE),]
  table(sent_post2014$SENTENCE_DATE)
  
  # Limit to unique Case participants in sentencing data
  sentence_race <- sent_post2014 %>% 
                    select(CASE_ID, CASE_PARTICIPANT_ID, RACE) %>% 
                    distinct(CASE_ID, CASE_PARTICIPANT_ID, RACE)
    
  
  cbind(table(sentence_race$RACE))
  
  sentence_race$RACE <- recode(sentence_race$RACE, "HISPANIC" = "Hispanic","ASIAN"="Asian" )
  
  sentence_race <- sentence_race %>% 
      filter(RACE == "Black" | RACE== "White" | RACE == "Hispanic" | RACE == "White [Hispanic or Latino]" | RACE == "Asian")
  table(sentence_race$RACE)
  race_counts <- prop.table(table(sentence_race$RACE))
  race_counts
  race_counts <- data.frame(race_counts)
  race_counts <- race_counts[which(race_counts$Freq!=0),]
  ggplot(race_counts,aes(Var1,Freq)) +
        geom_col() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Racial Breakdown of All Case Participants With Sentences") +
    ylim(0,1)
      
  
  # Keep participant IDs for people who got sentence type
  sentence1 <- sentence %>% 
    filter(SENTENCE_TYPE=="2nd Chance Probation")
  
  
  table(sentence1$RACE)
  sentence1$RACE <- recode(sentence1$RACE, "HISPANIC" = "Hispanic", "ASIAN"="Asian")
  
  sentence1 <- sentence1 %>% 
    filter(RACE == "Black" | RACE== "White" | RACE == "Hispanic" | RACE == "White [Hispanic or Latino]" | RACE == "Asian")
  race_counts2 <- prop.table(table(sentence1$RACE))
  race_counts2
  race_counts2 <- data.frame(race_counts2)
  race_counts2 <- race_counts2[which(race_counts2$Freq!=0),]
  ggplot(race_counts2,aes(Var1,Freq)) +
    geom_col() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Racial Breakdown of Second Chance Probation Case Participants With Sentences") +
    ylim(0,1)
  
  race_counts
  race_counts2
  
  View(counts)
  
colnames(sentence1)
length(unique(sentence$CHARGE_ID))
length(unique(sentence$CHARGE_VERSION_ID))
length(unique(sentence1$CHARGE_ID))
length(unique(sentence1$CASE_PARTICIPANT_ID))

sentence1 <- sentence1 %>% 
              select("CASE_ID","CASE_PARTICIPANT_ID","SENTENCE_TYPE")

sentence2 <- inner_join(sentence,sentence1,by=c("CASE_ID","CASE_PARTICIPANT_ID"))
table(sentence2$PRIMARY_CHARGE)
table(sentence2$SENTENCE_TYPE.x)
table(sentence2$SENTENCE_TYPE.y)

table(sentence1$SENTENCE_TYPE)
length(unique(sentence2$CASE_PARTICIPANT_ID))


# okay - in this data (old data), there is no interesting variation in the sentence types (1110/1112 are
# second chance probation). so i think its okay to just filter on primary charge so we dont have multiple charges
# per person





sentence2 <- sentence2 %>% 
              mutate(duplicates = duplicated(CASE_PARTICIPANT_ID))
sentence_dups <- sentence2 %>% 
          filter(duplicates==1)
sentence_only_dups <- inner_join(sentence2,sentence_dups,by=c("CASE_ID","CASE_PARTICIPANT_ID"))

table(sentence_only_dups$SENTENCE_TYPE.x.x)
table(sentence_only_dups$SENTENCE_TYPE.y.x)
table(sentence_only_dups$SENTENCE_TYPE.y.y)
table(sentence_only_dups$SENTENCE_TYPE.x.y)



# Turn RECEIVED_DATE from strings to date objects
intake <- intake %>%
  mutate(RECEIVED_DATE = as.Date(RECEIVED_DATE, '%m/%d/%Y'))

# Number of cases that get into the SA felony case management
# system every month. What does that mean? GOOD QUESTION!
monthly_felonies <- (intake %>% 
                       mutate(month = round_date(RECEIVED_DATE, "month")) %>%
                       group_by(month) %>%
                       summarize(felonies=n()))

intake_with_month = mutate(intake, month=round_date(RECEIVED_DATE, 'month'))

plot(felonies ~ month, data=monthly_felonies)
plot(felonies ~ month, data=monthly_felonies, type='l')

# There's been significant decline, but also weird stuff seems
# to be happening at beginning and end, let's trim that out

summary(monthly_felonies)

monthly_felonies <- filter(monthly_felonies, month > as.Date('2011-04-01'))
monthly_felonies <- filter(monthly_felonies, month < as.Date('2018-01-01'))

plot(felonies ~ month, data=monthly_felonies, type='l')
plot(felonies ~ month, data=monthly_felonies, type='l', ylim=c(0,5000))

# We are going to be doing more work with intake, so let's go ahead
# and trim the starting dataset

intake <- filter(intake, RECEIVED_DATE >= as.Date('2011-03-01'))
intake <- filter(intake, RECEIVED_DATE < as.Date('2018-01-01'))

# Now let's look at the number of cases that are directly filed by
# the police bodies in Cook County without felony review, and which
# result in a felony filing. These should just be felony drug cases

monthly_drug_felonies <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(FR_RESULT == '') %>%
                            mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                            group_by(month) %>%
                            summarise(drug_felonies = n_distinct(CASE_PARTICIPANT_ID)))

# We'll add these numbers to our total monthly numbers, lining up the
# data by month
monthly_felonies <- left_join(monthly_felonies, 
                              monthly_drug_felonies, by='month')

lines(drug_felonies ~ month, data=monthly_felonies, lty=2)

# It's hard to tell if the trend in drug cases has been different
# than the overall trend. To take a closer look let's see how proportion
# of drug cases has changed over time
plot(drug_felonies/felonies ~ month, data=monthly_felonies, type='l')

# So there's been a big drop looking at about the start of 2016. Let's
# dig in more by looking at diferent levels of charges within 
# felony drug cases
monthly_drug_felonies_4 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                              filter(Offense_Category == 'Narcotics') %>%
                              filter(CLASS == '4') %>%
                              filter(FR_RESULT == '') %>%
                              mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                              group_by(month) %>%
                              summarise(drug_felonies_4 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_3 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                              filter(Offense_Category == 'Narcotics') %>%
                              filter(CLASS == '3') %>%
                              filter(FR_RESULT == '') %>%
                              mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                              group_by(month) %>%
                              summarise(drug_felonies_3 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_2 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                              filter(Offense_Category == 'Narcotics') %>%
                              filter(CLASS == '2') %>%
                              filter(FR_RESULT == '') %>%
                              mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                              group_by(month) %>%
                              summarise(drug_felonies_2 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_1 <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                              filter(Offense_Category == 'Narcotics') %>%
                              filter(CLASS == '1') %>%
                              filter(FR_RESULT == '') %>%
                              mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                              group_by(month) %>%
                              summarise(drug_felonies_1 = n_distinct(CASE_PARTICIPANT_ID)))

monthly_drug_felonies_x <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                              filter(Offense_Category == 'Narcotics') %>%
                              filter(CLASS == 'X') %>%
                              filter(FR_RESULT == '') %>%
                              mutate(month = round_date(RECEIVED_DATE.x, "month")) %>%
                              group_by(month) %>%
                              summarise(drug_felonies_x = n_distinct(CASE_PARTICIPANT_ID)))

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_4,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_3,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_2,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_1,
                              by='month')

monthly_felonies <- left_join(monthly_felonies,
                              monthly_drug_felonies_x,
                              by='month')

# So Level 4 cases, which is the lowest severity class, makes up 
# the bulk of felony drug cases, and it seems that it's really 
# these cases that have dropped so much
plot(drug_felonies ~ month, 
     data=monthly_felonies, 
     type='l', 
     ylim=c(0, 2000))

lines(drug_felonies_4 ~ month, 
      data=monthly_felonies, 
      lty=2)

lines(drug_felonies_3 ~ month, 
      data=monthly_felonies, 
      lty=3)

lines(drug_felonies_2 ~ month, 
      data=monthly_felonies, 
      lty=4)

lines(drug_felonies_1 ~ month, 
      data=monthly_felonies, 
      lty=5)

lines(drug_felonies_x ~ month, 
      data=monthly_felonies, 
      lty=6)

plot(drug_felonies_4 ~ month, 
     data=monthly_felonies, 
     type='l', 
     ylim=c(0, 1500))

# Okay, let's switch gears and see how different races and ethnicities
# are being treated by the system

drug_felonies_by_race <- (inner_join(intake, initiation, by='CASE_PARTICIPANT_ID') %>% 
                            filter(Offense_Category == 'Narcotics') %>%
                            filter(FR_RESULT == '') %>% 
                            group_by(RACE.x) %>%
                            summarise(drug_felonies = n_distinct(CASE_PARTICIPANT_ID)))

drug_felonies_by_race <- arrange(drug_felonies_by_race, desc(drug_felonies))

drug_felonies_by_race