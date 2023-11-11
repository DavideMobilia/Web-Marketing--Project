## -------------------------------- CHURN MODEL ------------------------------- 

### ---- DATA PREPARATION ----
dt1_fidelity <- df_1_cli_fid_clean 
dt2_account <- df_2_cli_account_clean 
dt3_address <- df_3_cli_address_clean 
dt7_tickets <- df_7_tic_clean_final 

dt1_fidelity <- dt1_fidelity %>%
  mutate(LAST_COD_FID = as.factor(LAST_COD_FID))

dt3_address <- dt3_address %>%
  mutate(REGION = as.factor(REGION))

## select only purchases and not returns
dt7_tickets %>% filter(DIREZIONE == "1") -> dt7_buy


## new variable "days": difference of days between the observation date and the 
##    date of the last observation recorded
dt7_buy$days <- as.integer(difftime(time1 = "2019-05-01", # last date in dt
                                    time2 = dt7_buy$TIC_DATE,
                                    units = "days"))
summary(dt7_buy$days)

## Factor: 1 o 2 ->  if a purchase was made in the 1st or the 2nd semester
dt7_buy$semester[which(dt7_buy$days <= 182)] <- 2
dt7_buy$semester[which(dt7_buy$days > 182)] <- 1
dt7_buy <- dt7_buy %>%
  mutate(semester = as.factor(semester))


## Recency
cust_recency <- dt7_buy %>%
  group_by(ID_CLI) %>%
  summarise(ultimo_acquisto = min(days),
            primo_acquisto = max(days),
            semester) %>%
  ungroup() %>%
  as.data.frame()

## Frequency
cust_frequency <- dt7_buy %>%
  group_by(ID_CLI) %>%
  summarise(n_acquisti = n_distinct(ID_SCONTRINO)) %>%
  as.data.frame()

## calcolo Monetary
cust_monetary <- dt7_buy %>%
  group_by(ID_CLI) %>%
  summarise(spesa_lorda = sum(IMPORTO_LORDO),
            spesa_sconto = sum(SCONTO),
            spesa_netta = spesa_lorda - spesa_sconto) %>%
  as.data.frame()

## customers: all the clients 
customers <- merge(cust_recency, cust_monetary[, c("ID_CLI", "spesa_lorda", "spesa_sconto")], by = "ID_CLI")
customers <- merge(customers, cust_frequency, by = "ID_CLI")

## add relevant information from other dataframes to "customers" 
cust_region <- left_join(dt2_account[, c("ID_CLI", "ID_ADDRESS")],
                         dt3_address[, c("ID_ADDRESS", "REGION")], by = "ID_ADDRESS") # Region

customers <- left_join(customers, dt1_fidelity[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI") # Type of Fidelity Card
customers <- left_join(customers, cust_region, by = "ID_CLI")
customers$ID_ADDRESS <- NULL # remove ID_ADDRESS

customers <- na.omit(customers) # remove missing data

## threshold to define the churners: all the clients who made their first and 
##    last purchase further away than the 85% of the other customers
customers %>% 
  summarise(diff_days = primo_acquisto-ultimo_acquisto) -> dayDifference

churn_tr <- quantile(dayDifference$diff_days, 0.85)

## split the purchases between 1st and 2nd semester
active_1 <- customers %>%
  filter(semester == 1)
active_1 <- subset(active_1, !duplicated(active_1$ID_CLI))

active_2 <- customers %>%
  filter(semester == 2)
active_2 <- subset(active_2, !duplicated(active_2$ID_CLI))

## all the clients that made their first purchase more than 182 days ago (in 1st semester)
cust_old_1 <- customers %>%
  filter(primo_acquisto > 182) 
cust_old_1 <- subset(cust_old_1, !duplicated(cust_old_1$ID_CLI))

## split churners and actives:
##    churners: clients that made their first purchase in the 1st semester and 
##        made their last purchase before the threshold
##    active: clients that made a purchase in the 2nd semester and are not churners
##  group the purchases by clients
churners <- cust_old_1 %>% filter(ultimo_acquisto > churn_tr)
active <- anti_join(active_2, churners, by="ID_CLI")

### ---- MODEL ----

## add the churn column in model data (active clients in the 1st semester)
model_data <- active_1 %>% 
  mutate(churn = ifelse(active_1$ID_CLI %in% churners$ID_CLI, "1", "0")) %>%
  mutate(churn = as.factor(churn))

## remove unuseful variables from model_data
model_data <- select(model_data, -semester)
model_data <- select(model_data, -ultimo_acquisto)

## remove the ID numbers and save them in row.names
row.names(model_data) <- model_data$ID_CLI
model_data <- select(model_data, -ID_CLI)

## define train and test set
set.seed(12)
trainIndex <- createDataPartition(model_data$churn, 
                                  p = .75, 
                                  list = FALSE,
                                  times = 1)

## partition train and test set
train_data <- model_data[trainIndex, ]
test_data  <- model_data[-trainIndex, ]

table(train_data$churn) 

## undersampling because of unbalanced classes
churn_1 <- train_data %>% filter(churn == "1") 
churn_0 <- train_data %>% filter(churn == "0") 

churn_0_bal <- churn_0[sample(nrow(churn_0), nrow(churn_1)), ] 
train_data_bal <- rbind(churn_0_bal, churn_1)

### Models:

## random forest
rF <- randomForest(churn ~., data = train_data_bal, ntree = 100)
print(rF) 

pred_rf <- predict(rF, test_data[, -7], type = "class") # remove churn from prediction
cM_rf <- confusionMatrix(pred_rf, test_data$churn, mode = "prec_recall") # Accuracy: 0.922
cM_rf

varImpPlot(rF, 
           sort = T,
           main = "Importanza delle variabili in Random forest") 
## primo_acquisto is the most important variable


## regression tree
r_tree <- rpart(churn ~., data = train_data_bal)

rpart.plot(r_tree, extra = "auto")

summary(r_tree) # primo_acquisto is the most important variable
printcp(r_tree) 

pred_tree <- predict(r_tree, test_data[, -7], type = "class")
p_tree <- unlist(pred_tree)
cM_tree <- confusionMatrix(p_tree, test_data$churn, mode = "prec_recall") # Accuracy: 0.9123     
cM_tree

## logistic regression

logistic <- train(churn ~.,
                  data = train_data_bal,
                  method = "glm")
summary(logistic)

pred_log <- predict(logistic, test_data[, -7], type = "raw")
cM_log <- confusionMatrix(pred_log, test_data$churn, mode = "prec_recall") # Accuracy: 0.903
cM_log

### ---- MODEL PERFORMANCE ----

## Accuracy
accuracy <- as.data.frame(t(cbind(cM_rf$overall[1], cM_tree$overall[1], cM_log$overall[1])))
accuracy <- as.data.frame(cbind(c("Random forest", "Regression tree",
                                  "Logistic regression"), accuracy))
colnames(accuracy) <- c("Model", "Accuracy")

ggplot(data = accuracy, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_cartesian(ylim = c(0.80, 0.99)) +
  theme_minimal() +
  labs(title = "Accuracy",
       x = "Models",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # centering title
## best model in accuracy is random forest

## Lift
pred_class = as.data.frame(cbind(pred_rf, p_tree, pred_log))
pred_class = cbind(pred_class, test_data$churn)
colnames(pred_class) <- c("pred_rf", "pred_tree", "pred_log", "churn")
head(pred_class)

lift_rf = gain_lift(data = pred_class, score = "pred_rf", target = "churn")
lift_tree = gain_lift(data = pred_class, score = "pred_tree", target = "churn")
lift_log = gain_lift(data = pred_class, score = "pred_log", target = "churn")

## ROC
roc_rf <- roc.curve(test_data$churn, pred_rf, col=2, lwd=2)
roc_tree <- roc.curve(test_data$churn, pred_tree, add.roc=TRUE, col=3, lwd=2)
roc_log <- roc.curve(test_data$churn, pred_log, add.roc=TRUE, col=4, lwd=2)
roc_rf
roc_tree
roc_log
legend("bottomright", c("Random forest", "Regression tree", "Logistic regression"), col=2:4, lwd=2)

roc <- as.data.frame(t(cbind(roc_rf$auc, roc_tree$auc, roc_log$auc)))
colnames(roc) <- "AUC"
rownames(roc) <- c("Random forest", "Regression tree", "Logistic regression")
head(roc)
## best model is random forest also according to ROC and AUC


### ---- BEST MODEL APPLICATION ----

## the best model is Random forest
## apply the model to those clients who purchased in the 2nd semester
active_2$prob_to_churn <- predict(rF, active_2, type = "prob")[, 2]
head(active_2)

## mailList: those clients who have a churn probability over 50% are selected
active_2$mailList <- as.factor(ifelse(active_2$prob_to_churn > 0.5, 
                                      "probable churn", "loyal"))

## plot how many clients have been selected as probable churners
ggplot(data.frame(active_2), aes(x = mailList, y = n_distinct(ID_CLI))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Churn model",
       x = "Churners",
       y = "Total clients") +
  theme_minimal() +
  fill = "steelblue"
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) # centering title
