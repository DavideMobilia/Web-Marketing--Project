#### FIRST LOOK of df_1 ####

str(df_1_cli_fid)
summary(df_1_cli_fid)

#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI), "-", 
                                                 as.character(ID_FID)))
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates for combination CLI-FID !!!#

#### CLEANING DATA TYPES in df_1 ####

## formatting dates
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####

## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarise(NUM_FIDs =  n_distinct(ID_FID), 
            NUM_DATEs = n_distinct(DT_ACTIVE)
            )

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI) 

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#

## lets examine in detail clients with multiple subscriptions
num_fid_x_cli %>% filter(NUM_FIDs == 3)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# from last subscription   --> type of fidelity, status
df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# from subscriptions count --> number of subscriptions made
df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI,
         ID_FID,
         LAST_COD_FID = COD_FID,
         LAST_TYP_CLI_FID = TYP_CLI_FID,
         LAST_STATUS_FID = STATUS_FID,
         LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

#### EXPLORE COLUMNS of df_1 ####

### variable LAST_COD_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data = df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
         ) +
    geom_bar(stat = "identity"
             , fill = "steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

# TODO: EXPLORE the remaining df_1_cli_fid_clean relevant variables

### variable LAST_TYP_CLI_FID ###

## compute distribution
df1_dist_typ <- df_1_cli_fid_clean %>%
  group_by(LAST_TYP_CLI_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_typ

## plot distribution
plot_df1_dist_typ <- (
  ggplot(data= df1_dist_typ
         , aes(x=LAST_TYP_CLI_FID, y=TOT_CLIs)) +
    geom_bar(stat = "identity"
             , fill = "steelblue") +
    theme_minimal() +
    labs(y = "Number of clients", x = "Main account")
)

plot_df1_dist_typ


### variable LAST_STATUS_FID

## compute distribution
df1_dist_statfid <- df_1_cli_fid_clean %>%
  group_by(LAST_STATUS_FID ) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_statfid

## plot distribution
plot_df1_dist_statfid <- (
  ggplot(data = df1_dist_statfid
         , aes(x=LAST_STATUS_FID, y=TOT_CLIs)) +
    geom_bar(stat = "identity"
             , fill = "steelblue") +
    theme_minimal() +
    labs(y = "Number of clients", x = "Last status of fidelity")
)

plot_df1_dist_statfid


### variable NUM_FIDs  ###

## compute distribution
df1_dist_numfid <- df_1_cli_fid_clean %>%
  group_by(NUM_FIDs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_numfid

## plot distribution
plot_df1_dist_numfid <- (
  ggplot(data=df1_dist_numfid
         , aes(x=NUM_FIDs, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal() +
    labs(y = "Number of clients", x = "Number of fidelity per client")
)

plot_df1_dist_numfid

# ??? si potrebbero anche fare LAST_DT_ACTIVE (tipo quanto tempo è passato 
## dall'ultima attività, 1y, 6 mesi, 3 mesi); FIRST_ID_NEG; FIRST_DT_ACTIVE
## anche perchè le distribuzionni fatte sono 90%+ una e niente l'altra

#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)