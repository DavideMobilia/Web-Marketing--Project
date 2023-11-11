#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarise(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()
### mmmm ???? TOT_ID_ADDRESSes 361330 TOT_ROWs 361332

#### CLEANING DATA TYPES in df_3 ####

## format string as factors
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### CLEANING MISSING VALUES in df_3 ####

str(df_3_cli_address_clean)
df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS))

## lets examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarise(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### EXPLORE COLUMNS of df_3 ####
# TODO: EXPLORE the df_3_cli_address_clean relevant variables

### variable PRV (province) ###

## compute distribution
df3_dist_PROV <- df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarise(TOT_ADDRESs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDRESs/sum(TOT_ADDRESs)) %>%
  arrange(desc(PERCENT))

head(df3_dist_PROV,10)

## plot distribution
plot_df3_dist_prov <- (
  ggplot(data=head(df3_dist_PROV,10)
         , aes(x=reorder(PRV,-PERCENT), y=TOT_ADDRESs)) +
    geom_bar(stat = "identity"
             , fill = "steelblue") +
    theme_minimal() +
    labs(y = "Number of clients", x = "Province")
)

plot_df3_dist_prov


### variable REGION ###

## compute distribution  
df3_dist_region <- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarise(TOT_ADDRESs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDRESs/sum(TOT_ADDRESs)) %>%
  arrange(desc(PERCENT))

df3_dist_region

## plot distribution
plot_df3_dist_region <- (
  ggplot(data=head(df3_dist_region,10)
         , aes(x=reorder(REGION,-PERCENT), y=TOT_ADDRESs)) +
    geom_bar(stat = "identity"
             , fill = "steelblue") +
    theme_minimal() +
    labs(y = "Number of clients", x = "Region")
)

plot_df3_dist_region

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)