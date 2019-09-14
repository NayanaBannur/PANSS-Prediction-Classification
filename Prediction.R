# OBJECTIVE 3: Prediction of week 18 PANSS scores

require(xgboost)

# Load data : Read in csv files

df <- read.table("/Users/nayana/Desktop/Stanford/Data Mining/Final Project/Dataset/StudyABCD.csv", 
                 header = TRUE,
                 sep = ",")

df2 <- read.table("/Users/nayana/Desktop/Stanford/Data Mining/Final Project/Dataset/Study_E.csv", 
                 header = TRUE,
                 sep = ",")

testdata.raw <- read.table("/Users/nayana/Desktop/Stanford/Data Mining/Final Project/Dataset/testSet1.csv", 
                  header = TRUE,
                  sep = ",")

int1 <- model.matrix(~((factor(TxGroup) + VisitDay))^2 -1, df)
int2 <- model.matrix(~((factor(TxGroup) + VisitDay))^2 -1, df2)
intt <- model.matrix(~((factor(TxGroup) + VisitDay))^2 -1, testdata.raw)

df.select <- data.frame(df$PANSS_Total, df$TxGroup, df$VisitDay,
                        df$PatientID, df$SiteID, df$RaterID, int[,4], sqrt(df$VisitDay), 
                        #f1_1, f1_2, f1_3, f1_4,
                        df$Country, df$Study)

colnames(df.select) <- c("PANSS_Total", "TxGroup", "VisitDay",
                         "PatientID", "SiteID", "RaterID", "Int", "RootV", 
                         #"Ind_1","Ind_2","Ind_3","Ind_4",
                         "Country", "Study")

df2.select <- data.frame(df2$PANSS_Total, df2$TxGroup, df2$VisitDay, 
                         df2$PatientID, df2$SiteID, df2$RaterID, int2[,4], sqrt(df2$VisitDay), 
                         #f2_1, f2_2, f2_3, f2_4, 
                         df2$Country, df2$Study)

colnames(df2.select) <- c("PANSS_Total", "TxGroup", "VisitDay",
                          "PatientID", "SiteID", "RaterID", "Int", "RootV", 
                          #"Ind_1","Ind_2","Ind_3","Ind_4", 
                          "Country","Study")

df.final <- rbind(df.select, df2.select)

testdata <- data.frame(testdata.raw$PANSS_Total, testdata.raw$TxGroup, testdata.raw$VisitDay,
                       testdata.raw$PatientID, testdata.raw$SiteID, testdata.raw$RaterID, intt[,4],
                       sqrt(testdata.raw$VisitDay), 
                       testdata.raw$Country, testdata.raw$Study)

colnames(testdata) <- c("PANSS_Total", "TxGroup", "VisitDay",
                         "PatientID", "SiteID", "RaterID", "Int", "RootV","Country", "Study")

df.complete <- rbind(df.final, testdata)

set.seed(1)
train <- c(1 : nrow(df.final))

df.train <- df.complete[train, ]

train_y = df.train[,'PANSS_Total']

previous_na_action <- options('na.action')
options(na.action='na.pass')
df.enc <- sparse.model.matrix(PANSS_Total ~ .-1, data = df.complete)
options(na.action=previous_na_action$na.action)

dtrain <- df.enc[train,]
dtest <- df.enc[-train,]

param <- list(objective = "reg:linear",
              eval_metric = "rmse",
              max_depth = 10,
              eta = 0.01,
              subsample = 0.5
)
m.nround = 10000

bst <- xgboost(data = dtrain, label = train_y, params = param, nrounds = m.nround,
              verbose = T)

# importance <- xgb.importance(feature_names = colnames(df.enc.train), model = bst)
# print(importance)
# xgb.plot.importance(importance)

pred <- predict(bst, dtest)
op <- cbind(testdata$PatientID, pred)
write.table(op, "/Users/nayana/Desktop/Stanford/Data Mining/Final Project/sub3.csv", 
           sep = ",", row.names = F)


