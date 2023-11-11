#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor 
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

# TODO: EXPLORE the df_4_cli_privacy_clean relevant variables

### variable FLAG_PRIVACY_1 ###

## compute distribution
df1_dist_privacy1 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_privacy1

## plot distribution
plot_df1_dist_privacy1 <- (
  ggplot(data=df1_dist_privacy1
         , aes(x=FLAG_PRIVACY_1, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal() +
    labs(y = "Percentage of clients", x = "Privacy flag")
)

plot_df1_dist_privacy1


### variable FLAG_PRIVACY_2 ###

## compute distribution
df1_dist_privacy2 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_privacy2

## plot distribution
plot_df1_dist_privacy2 <- (
  ggplot(data=df1_dist_privacy2
         , aes(x=FLAG_PRIVACY_2, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal() +
    labs(y = "Percentage of clients", x = "Profiling flag")
)

plot_df1_dist_privacy2


### variable FLAG_DIRECT_MKT ###

## compute distribution
df1_dist_directMKT <- df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_directMKT

## plot distribution
plot_df1_dist_directMKT <- (
  ggplot(data=df1_dist_directMKT
         , aes(x=FLAG_DIRECT_MKT, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal() +
    labs(y = "Percentage of clients", x = "Direct marketing flag")
)

plot_df1_dist_directMKT


#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)