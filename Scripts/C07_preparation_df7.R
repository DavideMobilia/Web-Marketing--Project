#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarise(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill = DIREZIONE, x = TIC_HOUR, y = TOT_TICs)) +
    geom_bar(stat = "identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill = DIREZIONE, x = TIC_HOUR, y = TOT_TICs)) +
    geom_bar(stat = "identity", position = "fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill = DIREZIONE, x = COD_REPARTO, y = TOT_TICs)) +
    geom_bar(stat = "identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill = DIREZIONE, x = COD_REPARTO, y = TOT_TICs)) +
    geom_bar(stat = "identity", position = "fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data = df7_dist_datetyp
         , aes(fill = DIREZIONE, x = TIC_DATE_TYP, y = TOT_TICs)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("#023858", "#74a9cf"), labels = c("Resi", "Acquisti")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) # centering title
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill = DIREZIONE, x = TIC_DATE_TYP, y = TOT_TICs)) +
    geom_bar(stat = "identity", position = "fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarise(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data = df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color = DIREZIONE, x = IMPORTO_LORDO)) +
    geom_histogram(binwidth = 10, fill = "white", alpha = 0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data = df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color = DIREZIONE, x = SCONTO)) +
    geom_histogram(binwidth = 10, fill = "white", alpha = 0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### ???? TO DO df_7 ???? ####

### EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_dist_avgimportosconto_cod_rep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))%>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cod_rep

# plot Importo Lordo by Cod_Reparto
plot_df7_importolordo_codrep <- (
  ggplot(data = df7_dist_avgimportosconto_cod_rep, aes(fill = DIREZIONE,
                                                       x = COD_REPARTO,
                                                       y = AVG_IMPORTO_LORDO)) +
    geom_bar(stat = "identity") +
    theme_minimal()
)
plot_df7_importolordo_codrep 

# plot Sconto by Cod_Reparto
plot_df7_sconto_codrep <- (
  ggplot(data = df7_dist_avgimportosconto_cod_rep, aes(fill = DIREZIONE, 
                                                       x = COD_REPARTO, 
                                                       y = AVG_SCONTO)) +
    geom_bar(stat = "identity") +
    theme_minimal()
)
plot_df7_sconto_codrep


### EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO) !!!! 
df7_dist_idarticolo <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO,DIREZIONE) %>%
  summarize(NUM_VENDITE = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame() 

summary(df7_dist_idarticolo$NUM_VENDITE)

df7_dist_idarticolo %>%
  filter(DIREZIONE == 1) %>%
  select(NUM_VENDITE) %>%
  group_by(NUM_VENDITE) %>%
  count()
## The distribution is unbalanced towards 1. The majority of products is being
##    sold only once. We now analyze the best sellers

df7_dist_top_product_sells <-  df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_ARTICOLO) %>%
  summarize(NUM_VENDITE = n_distinct(ID_SCONTRINO)) %>%
  mutate(PERCENT = NUM_VENDITE/sum(NUM_VENDITE))%>%
  arrange(desc(PERCENT)) %>%
  filter(row_number() <= 10)%>%
  as.data.frame()

df7_dist_top_product_sells
## The article 33700716 on its own covers 1.5% of sales

## Prodotti piu' resituiti
df7_dist_top_product_refunds <-  df_7_tic_clean_final %>%
  filter(DIREZIONE == -1) %>%
  group_by(ID_ARTICOLO) %>%
  summarize(NUM_RESTITUZIONI = n_distinct(ID_SCONTRINO)) %>%
  mutate(PERCENT = NUM_RESTITUZIONI/sum(NUM_RESTITUZIONI))%>%
  arrange(desc(PERCENT)) %>%
  filter(row_number() <= 10)%>%
  as.data.frame()

df7_dist_top_product_refunds
## The article 48020504 is the most returned 


### EXPLORE average IMPORTO_LORDO and average SCONTO by ID_CLI
df7_dist_avgimportosconto_idcli<- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))%>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_idcli

## IMPORTO_LORDO by ID_CLI
ggplot(data = df7_dist_avgimportosconto_idcli %>% filter((AVG_IMPORTO_LORDO > -500) & (AVG_IMPORTO_LORDO < 500)), aes(fill = DIREZIONE, x= AVG_IMPORTO_LORDO)) +
  geom_histogram(binwidth = 10) +
  theme_minimal()

## SCONTO by ID_CLI
ggplot(data = df7_dist_avgimportosconto_idcli %>% filter((AVG_SCONTO > -100) & (AVG_SCONTO < 100)), aes(fill = DIREZIONE, x = AVG_SCONTO)) +
  geom_histogram(binwidth = 2) +
  theme_minimal()

## compute the distribution of customers by number of purchases
df7_dist_customers_purchases <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(NUM_PURCHASES = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(NUM_PURCHASES))

df7_dist_customers_purchases

ggplot(data = df7_dist_customers_purchases %>% filter(NUM_PURCHASES <= 20), 
       aes(x = NUM_PURCHASES)) +
  geom_histogram(binwidth = 1, fill="steelblue") +
  theme_minimal()

## compute the days for next purchase curve
data_for_next_purchase <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>% 
  select(ID_CLI,
         ID_ARTICOLO,
         ID_SCONTRINO,
         TIC_DATE,
         DIREZIONE)      
arrange(ID_CLI)

head(data_for_next_purchase, 10)

df_np <- data_for_next_purchase %>%
  group_by(ID_CLI) %>%
  mutate(lagged = lag(TIC_DATE, 1))

## sostituisco gli NA nelle date lagged con la data non laggata
df_np <- df_np %>%
  mutate(lagged = if_else(is.na(lagged), TIC_DATE, lagged))

df_np <- df_np %>%
  mutate(Diff = as.numeric(TIC_DATE) - as.numeric(lagged))
head(df_np)
## days between one purchase and the other for each client
## NB: 0 days clients are excluded because it means that either they only made
##    one purchase or that it is the first purchase ever
dummy = df_np %>%
  group_by(ID_CLI,TIC_DATE) %>%
  summarize(days_between = sum(Diff)) %>%
  slice(-1)
head(dummy, 20)

## Average of daays between one purchase and th enext by client
dummy = dummy %>%
  group_by(ID_CLI) %>%
  summarize(avg_days_between = round(mean(days_between),0))
head(dummy)
max(dummy$avg_days_between)

x <- as.data.frame(table(dummy$avg_days_between))
x$Perc <- x$Freq/sum(x$Freq)
x = x %>% 
  rename(Days_lastPurchase = Var1)

## cumulative percentage of Days_lastPurchase to find the treshold for identifying
##    inactive clients
## We initially set the limit at 90%
x$Cum_Perc = cumsum(x$Perc)
x[x$Cum_Perc > 0.899 & x$Cum_Perc < 0.901, ]

## treshold for inactive clients is Days_lastPurchase = 94
ggplot(x, aes(x = as.numeric(Days_lastPurchase), y = cumsum(Perc))) +
  labs(title = "Next Purchase Curve",
       x = "Last Purchase Date (Days)",
       y = "Cumulative Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +    # Centering Title
  scale_x_continuous(limits = c(0,185), breaks = seq(0, 185, 25)) +
  geom_vline(xintercept = 94, linetype = "dotted") +
  geom_line(size = 1)+
  geom_label(aes(94, 0.5), label = "Inactive Clients\nTreshold", 
             show.legend = FALSE)


#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)
