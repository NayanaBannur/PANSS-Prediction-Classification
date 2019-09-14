# OBJECTIVE 4: Classifying assessments as passed or not

library(xgboost)
library(Matrix)

# Load data

df.raw <- read.table("/Users/nayana/Desktop/Stanford/Data Mining/Final Project/Dataset/StudyABCD.csv", 
                     header = TRUE,
                     sep = ",")

df2.raw <- read.table("/Users/nayana/Desktop/Stanford/Data Mining/Final Project/Dataset/obj4.csv", 
                  header = TRUE,
                  sep = ",")

df <- df.raw[, c("Country", "TxGroup", "VisitDay", "P1", "P2", "P3", "P4", "P5", "P6", "P7",
                 "N1", "N2", "N3", "N4", "N5", "N6", "N7",
                 "G1", "G2", "G3", "G4", "G5", "G6", "G7",
                 "G8", "G9", "G10", "G11", "G12", "G13", "G14", "G15", "G16",
                 "LeadStatus")]

df2 <- df2.raw[, c("Country", "TxGroup", "VisitDay", "P1", "P2", "P3", "P4", "P5", "P6", "P7",
                 "N1", "N2", "N3", "N4", "N5", "N6", "N7",
                 "G1", "G2", "G3", "G4", "G5", "G6", "G7",
                 "G8", "G9", "G10", "G11", "G12", "G13", "G14", "G15", "G16",
                 "LeadStatus")]

df$LeadStatus <- ifelse(df$LeadStatus == "Passed", 0, 1)

df.final <- rbind(df, df2)

set.seed(1)
train <- c(1 : nrow(df))

df.train <- df.final[train, ]
train_y <- df.train[,'LeadStatus']

previous_na_action <- options('na.action')
options(na.action='na.pass')
df.enc <- sparse.model.matrix(LeadStatus ~ .-1, data = df.final)
options(na.action = previous_na_action$na.action)

dtrain <- df.enc[train,]
dtest <- df.enc[-train,]

param <- list(objective = "binary:logistic",
              eval_metric = "logloss",
              max_depth = 15,
              eta = 0.001,
              subsample = 0.5,
              colsample_bytree = 0.5
)

m.nround <- 10000

bst <- xgboost(data = dtrain, label = train_y, params = param, nrounds = m.nround,
               verbose = T)

# importance <- xgb.importance(feature_names = colnames(dtrain), model = bst)
# print(importance)

library(MLmetrics)

pred.test <- predict(bst, newdata = dtest, n.trees = 10000)

op <- data.frame("AssessmentID" = df2.raw$AssessmentiD, "LeadStatus" = pred.test)
write.table(op, "/Users/nayana/Desktop/Stanford/Data Mining/Final Project/sub4.csv", 
            sep = ",", row.names = F)

