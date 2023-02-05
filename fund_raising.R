# Assignment 2

library("dplyr")
library("xgboost")
library("caret")

# -------------------------- functions --------------------------
confusion=function(truth,pred,conversion=c(1,1)){
  # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
  # pred: is a vector of predictions (coded 0,1 or TRUE,FALSE) for the same set, and in the same order
  # conversion: ratios of the proportion for each category before and after over/undersampling.
  # Output: Confusion matrix and fit statistics
  
  #  pred=factor(as.logical(pred),c(FALSE,TRUE))
  #  truth=factor(as.logical(truth),c(FALSE,TRUE))
  a=conversion*table(truth,pred,dnn=c("Truth","Prediction"))
  if(ncol(a)<2){ return(  list(
    Confusion=NA,
    Misclassification=NA,
    Precision=NA,
    Sensitivity=NA,
    Specificity=NA
  )
  )
  }
  list(
    Confusion=addmargins(a, FUN = list(Total = sum), quiet = TRUE),
    Misclassification=1-sum(diag(a))/sum(a),
    Precision=a[2,2]/sum(a[,2]),
    Sensitivity=a[2,2]/sum(a[2,]),
    Specificity=a[1,1]/sum(a[1,])
  )
  
}


roc=function(truth,p,k=100,plot=TRUE,lines=FALSE,...){
  # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
  # p: is a vector of predicted probabilites of getting 1 (or TRUE) for the same set, and in the same order
  # k: number of points at which the ROC curve will be evaluated
  # plot: plots the ROC curve (or not)
  # lines: add a line to an existing ROC plot
  # Output: (invisible means it will not be displayed on the console)
  #    ROC = list of points of the ROC curve
  #    AUC = Area under the curve
  
  Curve=rbind(c(0,0),
              t(sapply(quantile(p,(k:1)/(k+1)),function(th){
                pred=as.numeric(p>th)
                if(length(unique(pred))==2){
                  a=confusion(truth,pred)
                  return(c(1-a$Specificity,a$Sensitivity))
                } else {
                  return(c(NA,NA))
                }
              }
              )),
              c(1,1)
  )
  Curve=Curve[complete.cases(Curve),]
  if(plot&!lines) plot(Curve,xlab="1-Specificity",ylab="Sensitivity",main="ROC curve",xlim=0:1,ylim=0:1,type="l",...)
  if(plot&lines) lines(Curve,...)
  invisible(list(ROC=Curve,AUC=sum(diff(Curve[,1])*(Curve[-1,2]+Curve[-nrow(Curve),2])/2)))
}

lift=function(truth,p,k=100,plot=TRUE,lines=FALSE,...){
  # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
  # p: is a vector of predicted probabilites of getting 1 (or TRUE) for the same set, and in the same order
  # k: number of points at which the lift chart will be evaluated
  # plot: plots the lift chart (or not)
  # lines: add a line to an existing lift chart
  # Output: (invisible means it will not be displayed on the console)
  #     - list of points of the lift chart
  
  Curve=cbind((1:k)/k,c(sapply(quantile(p,((k-1):1)/k),function(th){confusion(truth,as.numeric(p>=th))$Precision})/mean(truth),1)
  )
  if(plot&!lines) plot(Curve,xlab="Depth",ylab="Cumulative Lift",main="Cumulative Lift Chart",xlim=0:1,type="l",...)
  if(plot&lines) lines(Curve,...)
  invisible(Curve)
}

# function that gives 0 or 1 according to a certain threshold: if p>=th ==> 1 else ==>0
p_to_binary <- function(prob,th){
  bin = ifelse(prob>=th,1,0)
  return(as.factor(bin))
}

# function to find the best cut
best_cut <- function(truth, probs){
  outs = confusion(truth,p_to_binary(probs,0))
  Misclassification=outs$Misclassification
  TP=outs$Sensitivity
  TN=outs$Specificity
  steps = seq(from = 0.01, to = max(probs), by= 0.01)
  for (p in steps){
    outs = confusion(truth,p_to_binary(probs,p))
    Misclassification = cbind(Misclassification,outs$Misclassification)
    TP = cbind(TP,outs$Sensitivity)
    TN = cbind(TN,outs$Specificity)
  }
  print(paste("length of Miss.",length(Misclassification),sep=" "))
  print(paste("length of TP:", length(TP),sep=" "))
  print(paste("length of TN:", length(TN),sep = " "))
  # plot on a chart
  p_cut = seq(from = 0, to = max(probs), by= 0.01)
  print(paste("length of pcut: ",length(p_cut),sep = " "))
  par(mfrow=c(3,1))
  plot(p_cut,Misclassification,type="l")
  plot(p_cut,TP,type="l")
  plot(p_cut,TN,type = "l")
}

# --------------------------------------------------------------------------------------------------------------

# load data
main_path = "C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Fall Term 2021\\statistical learning MATH 60603\\assignments\\Assignment 2 fundraising\\Data\\"

donation_history = read.csv(paste(main_path,"DonationHistory.csv",sep=""))
list_contacted2020 = read.csv(paste(main_path,"ListContacted2020.csv",sep=""),header = FALSE)
members_list = read.csv(paste(main_path,"MembersList.csv",sep=""))
news_letter_read = read.csv(paste(main_path,"NewsLetterRead.csv",sep=""))
sample_questionnaire = read.csv(paste(main_path,"Sample-Big5Questionnaire.csv",sep=""))
social_network_usage = read.csv(paste(main_path,"SocialNetworkUsage.csv",sep=""))

str(donation_history)
str(list_contacted2020)
str(members_list)
str(news_letter_read)
str(sample_questionnaire)
str(social_network_usage)

# -------------------------- Build the database --------------------------
df = members_list

# check if there were people that joined in 2020 and were contacted (just to understand the data)
members_list %>% select(ID,Joined) %>% filter(ID%in%list_contacted2020[,1]&&Joined == 2020) %>% nrow()
      # none of the people that joined in 2020 were contacted

max(members_list$Joined) # 2019
      # none of the people joined in 2020

# add a column "contacted" to df representing whether the indvidual was contacted or not
df$contacted = ifelse(df$ID %in% list_contacted2020[,1],1,0)

# exploit the donation history data
min(donation_history$Yr) # 2009
max(donation_history$Yr) # 2020
        # ==> add columns from 2009 to 2020
for(i in min(donation_history$Yr):max(donation_history$Yr)){
 df[[paste("donated_in_", i, sep = "")]] = ifelse(df$ID %in% (donation_history %>% filter(Yr == i))$ID, 1, 0)
}
# # verify
# df %>% select(ID, donated_in_2009:donated_in_2020)%>% filter(ID == 5000123) # ans: 2012, 2013, 2014, 2017, 2018, 2019

# calculate the average amount per ID over the years
average_amount = aggregate(donation_history$Amount, list(donation_history$ID), FUN=mean)
colnames(average_amount) = c("ID","AVG_amount")
df = merge(x = df, y = average_amount, by = "ID", all.x = TRUE)
df$AVG_amount[is.na(df$AVG_amount)]=0 # replace na with 0

# here we add the variable representing how many times did the user check the newsletter in a year
view_count = rowSums(news_letter_read[,-1]) # sum of the times he read the newspaper
news_letter_read = cbind(news_letter_read,view_count) # add the column to the news_letter_read dataset

# add the view_count to the df dataset
df = merge(df, news_letter_read %>% select(email, view_count), by = "email")

# -------------------------- UPLIFT model --------------------------
  # select only the individuals who have a donation history
  df_model = df %>% filter(ID %in% donation_history$ID)

  # train test split
  set.seed(65234897)
  train_percentage = 0.8
  trainID = sample(df_model$ID,ceiling(train_percentage*nrow(df_model)))
  valID = subset(df_model$ID, !(df_model$ID %in% trainID))
  df_train = df_model  %>% filter(ID %in% trainID)
  df_val = df_model  %>% filter(ID %in% valID)
  
  df_val_clean = df_val %>% select(-ID,-LastName,-FirstName,-email)

  # contacted = 1
  df_train_con = df_train %>% select(-ID,-LastName,-FirstName,-email) %>% filter(contacted == 1)
    # model 1: simple logistic regression
  m1con = glm(donated_in_2020~.-contacted, data=df_train_con, family = "binomial")
  p1con = predict(m1con,newdata=df_val_clean,type="response")
  roc(df_val_clean$donated_in_2020,p1con,col = "blue")$AUC
  
    # model 2: logistic regression, xgboost
  dmycon <- dummyVars(" ~ .", data = df_train_con)
  trsf_train_con <- data.frame(predict(dmycon, newdata = df_train_con))
  trsf_validate_con <- data.frame(predict(dmycon, newdata = df_val_clean))
  
  xgbcon <- xgboost(data = as.matrix(trsf_train_con%>%select(-donated_in_2020)), label = as.matrix(trsf_train_con$donated_in_2020), max_depth = 10, eta = 0.8, nthread = 2, nrounds = 10, objective = "binary:logistic")
  p2con = predict(xgbcon,newdata=as.matrix(trsf_validate_con%>%select(-donated_in_2020)),type="response")
  roc(df_val_clean$donated_in_2020,p2con,col = "red")$AUC
  
  
    # model 3: trees, xgboost
  
  
  best_mod_con = xgbcon
  pcon = p2con
  
  # contacted = 0  
  df_train_notcon = df_train %>% select(-ID,-LastName,-FirstName,-email) %>% filter(contacted == 0)
    # model 1: simple logistic regression
  m1notcon = glm(donated_in_2020~.-contacted, data=df_train_notcon, family = "binomial")
  p1notcon = predict(m1notcon,newdata=df_val_clean,type="response")
  roc(df_val_clean$donated_in_2020,p1notcon,col = "blue")$AUC
  
    # model 2: logistic regression, xgboost
  dmynotcon <- dummyVars(" ~ .", data = df_train_notcon)
  trsf_train_notcon <- data.frame(predict(dmynotcon, newdata = df_train_notcon))
  trsf_validate_notcon <- data.frame(predict(dmynotcon, newdata = df_val_clean))
  
  xgbnotcon <- xgboost(data = as.matrix(trsf_train_notcon%>%select(-donated_in_2020)), label = as.matrix(trsf_train_notcon$donated_in_2020), max_depth = 10, eta = 0.8, nthread = 2, nrounds = 10, objective = "binary:logistic")
  p2notcon = predict(xgbnotcon,newdata=as.matrix(trsf_validate_notcon%>%select(-donated_in_2020)),type="response")
  roc(df_val_clean$donated_in_2020,p2notcon,col = "blue")$AUC
  
  
  
    # model 3: trees, xgboost
  
  

  best_mod_notcon = xgbnotcon
  pnotcon = p2notcon
  
  # uplift plot
  uplift=pcon-pnotcon
  colup=colorRampPalette(c("Red","Green"))
  plot(p1notcon,p1con,xlab="Prob(donate) no contact",ylab="Prob(donate) with contact",xlim=0:1,ylim=0:1,col=colup(100)[ceiling(100*(uplift+1)/2)],pch=20)
  abline(a=0,b=1,lty=2)
  
  # -------------------------- where to cut --------------------------
    # i have to choose the right uplift value to cut at to maximize the revenues
    
  best_cut(truth = df_val$donated_in_2020, probs = uplift)
  
  # threshold uplift = 0
  thr_uplift = 0.1
  
  # -------------------------- Real-deal --------------------------
  df_model_removed = df_model %>% select(-ID,-LastName,-FirstName,-email)
  uplift_on_data = predict(best_mod_con, newdata = df_model_removed, type = "response") - predict(best_mod_notcon, newdata = df_model_removed, type = "response")
  ID_tobe_contacted = df_model$ID[which(uplift_on_data > thr_uplift)]
  path_export = 
    write.csv(ID_tobe_contacted,"C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Fall Term 2021\\statistical learning MATH 60603\\assignments\\Assignment 2 fundraising\\output01_xgb.csv", row.names = FALSE)
 
  # -------------------------- Real-deal for xgboost --------------------------
  df_model_removed = df_model %>% select(-ID,-LastName,-FirstName,-email)
   dmy <- dummyVars(" ~ .", data = df_model_removed)
   trsf <- data.frame(predict(dmy, newdata = df_model_removed))
  uplift_on_data = predict(best_mod_con, newdata = as.matrix(trsf%>%select(-donated_in_2020)), type = "response") - predict(best_mod_notcon, newdata = as.matrix(trsf%>%select(-donated_in_2020)), type = "response")
  ID_tobe_contacted = df_model$ID[which(uplift_on_data > thr_uplift)]
  path_export = 
    write.csv(ID_tobe_contacted,"C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Fall Term 2021\\statistical learning MATH 60603\\assignments\\Assignment 2 fundraising\\output001_xgb.csv", row.names = FALSE)
  
  
#-------------------------------------------------------------------------------------------------------------------#
  
    # -------------------------- amount donated --------------------------
  # in this part i will fit a model that predicts the amount donated
  # for each individual in donation history who donated in 2020, match his information from df_model
  
  df_amount = merge(df_model, donation_history, by = "ID")
  df_amount2020 = df_amount %>% filter(Yr == 2020)
  
  # select only the individuals who donated
  df_amoun2020 = df_amount2020 %>% filter(Amount > 0)
  
  # train-test split
  train_percentage_amount = 0.8
  trainID_amount = sample(df_amount2020$ID,ceiling(train_percentage_amount*nrow(df_amount2020)))
  valID_amount = subset(df_amount2020$ID, !(df_amount2020$ID %in% trainID_amount))
  df_train_amount = df_amount2020  %>% filter(ID %in% trainID_amount)
  df_val_amount = df_amount2020  %>% filter(ID %in% valID_amount)
  
  # fit linear regression model predicting the amount using xgboost
  train_x = data.matrix(df_train_amount %>% select(-ID,-LastName,-FirstName,-email,-Yr,-Amount))
  train_y = data.matrix(df_train_amount$Amount)
  
  test_x = data.matrix(df_val_amount %>% select(-ID,-LastName,-FirstName,-email,-Yr,-Amount))
  test_y = data.matrix(df_val_amount$Amount)
  
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  watchlist = list(train=xgb_train, test=xgb_test)
  model = xgb.train(data = xgb_train, max.depth = 60, watchlist=watchlist, nrounds = 500,booster = "gblinear", objective = "reg:linear")
  
  