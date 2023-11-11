## ------------------------ MARKET BASKET ANALYSIS ----------------------------- 

### ---- DATA PREPARATION ----
dt7_mba_tickets <- df_7_tic_clean_final 

## We find the top 100 selling products
count_tickets <- dt7_mba_tickets %>%
  group_by(ID_ARTICOLO) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_max(count, n = 100)

## Plot top 10 selling products
count_tickets %>%
  slice_max(count, n = 10) %>%
  ggplot(aes(x = reorder(ID_ARTICOLO, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # centering title
  labs(title = "Top 10 Best Sellers",
       x = "Article",
       y = "Total Purchases")


### ---- MODEL ----

## Association rule
tickets_ordered <- dt7_mba_tickets[order(dt7_mba_tickets$ID_CLI), ]

itemList <- plyr::ddply(dt7_mba_tickets, c("ID_CLI", "ID_SCONTRINO"),
                        function(df1)paste(df1$ID_ARTICOLO, collapse = ","))
itemList$ID_ARTICOLO <- NULL
itemList$TIC_DATE <- NULL
colnames(itemList) <- c("items", "ID_SCONTRINO", "V1")

write.csv(itemList, file.path(data_dir, "market_basket.csv"),
          quote = FALSE, row.names = TRUE)

## Read the transactions
tmp_data_dir <- paste(data_dir, "/market_basket.csv", sep="")

Rtrans <- arules::read.transactions(tmp_data_dir, format = 'basket', sep = ',', 
                                   rm.duplicates = TRUE)
summary(Rtrans)

## Item frequency absolute 
itemFrequencyPlot(Rtrans, topN = 10, type = "absolute", main = "Top 10 best sellers",
                  ylab = "Numbers of purchase", xlab = "Articles", 
                  col = hcl.colors(10, palette = "PuBu"))

## Item frequency relative
itemFrequencyPlot(Rtrans, topN = 5, type = "relative", xlab = "Articles", 
                  ylab = "Frequency (relative)", main = "Relative item frequency",
                  col = hcl.colors(5, palette = "PuBu"))

## Find frequent item sets
rules <- apriori(Rtrans, parameter = list(supp = 0.001, conf = 0.8))
rules <- sort(rules, by = "confidence", decreasing = TRUE)
summary(rules)


### ---- DATA EXPLORATION ----

## Scatter plot for the first 10 rules
topRules <- rules[1:10]
inspect(topRules)
plot(topRules) + 
  theme_minimal() +
  geom_point() + 
  scale_colour_gradientn(colours = hcl.colors(10, palette = "Blues")[0:5]) +
  theme(plot.title = element_text(hjust = 0.5)) # centering title

## Graph of the first Top 10 rules
plot(topRules, method = "graph") + 
  theme_minimal() +
  scale_colour_gradientn(colours = hcl.colors(10, palette = "Blues")[0:5]) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
