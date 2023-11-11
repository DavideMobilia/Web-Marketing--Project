#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

#### EXPLORE COLUMNS of df_5 ####
# TODO: EXPLORE the df_3_cli_address_clean relevant variables

### variable TYP_CAMP ###

## compute distribution 
df5_dist_typeCamp <- df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarize(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT))

df5_dist_typeCamp

## plot distribution
plot_df5_dist_typeCamp <- (
  ggplot(data=df5_dist_typeCamp
         , aes(x=reorder(TYP_CAMP,-PERCENT), y=TOT_CAMPs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal() +
    labs(y = "Numerosity", x = "Type of campaign")
)

plot_df5_dist_typeCamp

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)