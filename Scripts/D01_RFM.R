## --------------------------------- RFM MODEL --------------------------------- 

### ---- DATA PREPARATION ----

## We select only a portion of our customer base, we select only the clients that have made a 
## purchase (Direzione = 1) in the most recent period of time: from 01/01/2019 onwards (30-04-2019)
rfm_df <- df_7_tic_clean %>%
  filter(DIREZIONE == 1 , TIC_DATE > as.Date("01/01/2019", format = "%d/%m/%Y")) 

## To have a more precise estimation of the monetary value we calculate the revenue as
## (importo-sconto) for each purchase adding a new column                                                         
rfm_df$Revenue <- rfm_df$IMPORTO_LORDO - rfm_df$SCONTO

## Calculate the total revenue per client
rfm_df <- rfm_df %>%  
  group_by(ID_CLI, ID_SCONTRINO, TIC_DATE)  %>% 
  summarise(totRev = sum(Revenue)) %>% 
  select(ID_CLI, ID_SCONTRINO, TIC_DATE, totRev)

head(rfm_df)

### ---- RECENCY ----

# RECENCY: How recently the customer purchased as days from last purchase
# NB: We consider only the active customers as seen in <C07_preparation_df7.R> 
# where we defined the threshold for the inactive customer base as 94 days

#selecting last purchase date
recency <- rfm_df %>% 
  group_by(ID_CLI)  %>% 
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

# calculating the days passed after the last purchase considering 01/05/2019 as today date
recency$RECENCY <- difftime(as.Date("01/05/2019",
                                    format = "%d/%m/%Y"),       
                            recency$LAST_PURCHASE_DATE,
                            units = "days")
length(recency$ID_CLI)

# filtering the total customers 113106 for the active customers
recency <- recency %>% 
  filter(RECENCY <= 94)
length(recency$ID_CLI)
# active customer result in 99004 i.e. we have excluded 14.102 inactive clients


## Create 3 different client groups based on the recency distribution
recency <- within(recency,
                  recency_CLASS <- cut(as.numeric(recency$RECENCY),
                                       breaks = quantile(recency$RECENCY,
                                                         probs = c(0, .25, .75, 1)), 
                                       include.lowest = T,
                                       labels = c("low", "medium", "high")))         
table(recency$recency_CLASS)

#NB: Filter the rfm dataset to keep only the active clients
rfm_df_filtered = rfm_df %>%
  filter(ID_CLI %in% recency$ID_CLI)


### ---- FREQUENCY ----

# Indicates the  number of purchases in the date range considered
frequency <- rfm_df_filtered %>%
  group_by(ID_CLI) %>% 
  summarise(TOT_PURCHASE = length(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))

# We split the client base in 3 different classes
quantile(frequency$TOT_PURCHASE, probs = seq(0, 1, 1/20))
# q0 e q1 has the same value -> can't be used with cut function -> define manually 
# the breaks based on the quantiles 
frequency <- within(frequency,
                    frequency_CLASS <- cut(as.numeric(frequency$TOT_PURCHASE),
                                           breaks = c(0 , 2 , 4 , 101), 
                                           include.lowest = T, right = F,
                                           labels = c("low", "medium", "high")))
table(frequency$frequency_CLASS)


### ---- MONETARY ----

# Monetary indicates how much a client spend
monetary <- rfm_df_filtered %>%
  group_by(ID_CLI) %>% 
  summarise(tot = sum(totRev)) %>%
  ungroup() %>%
  as.data.frame() %>%                    
  arrange(desc(tot))
table(monetary$monetary_CLASS)

## We create 3 different classes
monetary <- within(monetary,
                   monetary_CLASS <- cut(as.numeric(monetary$tot),
                                         breaks = quantile(monetary$tot,
                                                           probs = c(0, .25, .75, 1)),
                                         include.lowest = T,
                                         labels = c("low", "medium", "high")))

## We merge all the information gathered
rfm_result <- merge(recency, frequency, by = "ID_CLI") 
rfm_result <- merge(rfm_result, monetary, by = "ID_CLI") 


### ---- MODEL ----

#Creating the RF classification based on the recency-frequency groups
rfm_result <- rfm_result %>%
  mutate(RF = case_when((recency_CLASS == "low") & (frequency_CLASS == "low") ~ "One-Timer",
                        (recency_CLASS == "medium") & (frequency_CLASS == "low") ~ "One-Timer",
                        (recency_CLASS == "high") & (frequency_CLASS == "low") ~ "Leaving",
                        (recency_CLASS == "low") & (frequency_CLASS == "medium") ~ "Engaged",
                        (recency_CLASS == "medium") & (frequency_CLASS == "medium") ~ "Engaged",
                        (recency_CLASS == "high") & (frequency_CLASS == "medium") ~ "Leaving",
                        (recency_CLASS == "low") & (frequency_CLASS == "high") ~ "Top",
                        (recency_CLASS == "medium") & (frequency_CLASS == "high") ~ "Top",
                        (recency_CLASS == "high") & (frequency_CLASS == "high") ~ "Leaving Top")) %>%
  mutate(RF = as.factor(RF))


# Creating RFM classification combining RF groups with the monetary class
rfm_result <- rfm_result %>%
  mutate(RFM = case_when((RF == "One-Timer") & (monetary_CLASS == "low") ~ "Cheap",
                         (RF == "One-Timer") & (monetary_CLASS == "medium") ~ "Tin",
                         (RF == "One-Timer") & (monetary_CLASS == "high") ~ "Copper",
                         (RF == "Leaving") & (monetary_CLASS == "low") ~ "Tin",
                         (RF == "Leaving") & (monetary_CLASS == "medium") ~ "Copper",
                         (RF == "Leaving") & (monetary_CLASS == "high") ~ "Bronze",
                         (RF == "Engaged") & (monetary_CLASS == "low") ~ "Copper",
                         (RF == "Engaged") & (monetary_CLASS == "medium") ~ "Bronze",
                         (RF == "Engaged") & (monetary_CLASS == "high") ~ "Silver",
                         (RF == "Leaving Top") & (monetary_CLASS == "low") ~ "Bronze",
                         (RF == "Leaving Top") & (monetary_CLASS == "medium") ~ "Silver",
                         (RF == "Leaving Top") & (monetary_CLASS == "high") ~ "Gold",
                         (RF == "Top") & (monetary_CLASS == "low") ~ "Silver",
                         (RF == "Top") & (monetary_CLASS == "medium") ~ "Gold",
                         (RF == "Top") & (monetary_CLASS == "high") ~ "Diamond")) %>%
  mutate(RFM = as.factor(RFM))


### ---- DATA EXPLORATION ----

# RECENCY
plot_recency <- rfm_result %>% 
  group_by(recency_CLASS) %>% 
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>% 
  ggplot(aes(x = recency_CLASS, y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c("darkorange", "steelblue", "brown")) +
  labs(title = "Recency distribution",
       x = "Recency classes",
       y = "Number of clients") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # centering title
plot_recency

#  FREQUENCY
plot_frequency <- rfm_result %>%
  group_by(frequency_CLASS) %>% 
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>% 
  ggplot(aes(x = frequency_CLASS, y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c("darkorange", "steelblue", "brown")) +
  labs(title = "Frequency distribution",
       x = "Frequency classes",
       y = "Number of clients") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # centering title    
plot_frequency

#  MONETARY
plot_monetary <- rfm_result %>% 
  group_by(monetary_CLASS) %>% 
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>% 
  ggplot(aes(x = monetary_CLASS, y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c("darkorange", "steelblue", "brown")) +
  labs(title = "Monetary distribution",
       x = "Monetary classes",
       y = "Number of clients") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # centering title    
plot_monetary

# Model RF distribution
plot_RF <- rfm_result %>% 
  group_by(RF) %>%
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>% 
  ggplot(aes(x = factor(RF, levels = c("One-Timer", "Leaving", "Engaged", "Top",
                                       "Leaving Top")), 
             y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c("darkorange", "steelblue", "seagreen",
                                       "brown", "mediumvioletred")) +
  labs(title = "RF distribution",
       x = "RF classes",
       y = "Number of clients") +
   theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # centering title 
plot_RF



# RFM model distribution
plot_rfm <- rfm_result %>% 
  group_by(RFM) %>%
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>%
  ggplot(aes(x = factor(RFM, levels = c("Cheap", "Tin", "Copper","Bronze", 
                                        "Silver", "Gold", "Diamond")),
             y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c("steelblue", "hotpink2", "seagreen", 
                                       "mediumvioletred", "darkorange", "tomato2", 
                                       "brown")) +
  labs(title = "RFM model",
       x = "RFM classes",
       y = "Number of clients") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # centering title 
plot_rfm
