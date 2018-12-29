#   This is a machine learning made in order to build a model of lottery correct numbers.
#   The lottery about we'll make the model is called "Lotofacil". It basically works with a choosing
# by lot, which has 25 numbers in total but only 15 chosen, if you get 11, 12, 13, 14 or 15
# you'll win a prize, the worst one is given by 11 correct chooses and the better by 15.
#   Project made by: Guilherme Bromonschenkel

# -> Reading data
lotofacil = read.csv("lotofacil_resultados.csv", sep = ";")

# -> Data normalization:
{
  # Changing column names (some of them is based in the order the ball was raffled):
  colnames(lotofacil) = c("Concurso", "Data", "1º", "2º", "3º", "4º", "5º", "6º", "7º", "8º", 
                          "9º", "10º", "11º", "12º", "13º", "14º", "15º")
  # Taking just the chosen numbers into a vector
  balls_chosen = lotofacil[3:17]
  # Made this variable to use on the data visualization below
  balls_chosen1 = balls_chosen
  # Making a balls_chosen sorted out variable
  balls_chosen_sorted = balls_chosen
  for(i in 1:nrow(balls_chosen)){
    balls_chosen_sorted[i,] = sort(balls_chosen[i,], decreasing = FALSE)
  }
  # Making a variable with the wrong numbers of every competition
  {
    balls_wrong = as.integer(c())
    total_balls = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,  13, 14, 
                    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
    for(i in nrow(balls_chosen):1){
    balls_wrong = rbind(setdiff(total_balls, balls_chosen_sorted[i,]), balls_wrong)
    }
  }
  # Filling in the variable of wrong numbers just to get the same shape of right numbers
  {
    # Creating some columns to data
    balls_wrong_complete = balls_wrong
    for(i in 1:5){
          balls_wrong_complete = cbind(balls_wrong_complete, 0)
    }
    # Putting random values to the new columns
    for(i in 11:15){
        for(j in 1:nrow(balls_chosen)){
          r = sample(1:10, 1)
          balls_wrong_complete[j,i] = setdiff(total_balls, balls_wrong_complete[j,])[r]
        }
    }
  }
  # Makes a column responsible for classifing if someone won or not
  {
    balls_wrong_complete = cbind(balls_wrong_complete, 0)
    colnames(balls_wrong_complete) = c("1º", "2º", "3º", "4º", "5º", "6º", "7º", "8º", 
                                       "9º", "10º", "11º", "12º", "13º", "14º", "15º", "Winner")
    balls_chosen = cbind(balls_chosen, 1)
    colnames(balls_chosen) = c("1º", "2º", "3º", "4º", "5º", "6º", "7º", "8º", 
                               "9º", "10º", "11º", "12º", "13º", "14º", "15º", "Winner")
  }
  # That's the final variable (after normalizing process) we'll use to make the model
  balls_normalized = rbind(balls_chosen, balls_wrong_complete)
  # Randomly sampling the dataset to help later on the modeling
  random_balls_normalized = balls_normalized[sample.int(nrow(balls_normalized))[1:nrow(balls_normalized)],]
  # Checking if there's no NA value on the variable
  sum(is.na(random_balls_normalized))
}

# -> Data visualization:
# - Correct Numbers Counting (without order distinction):
{
  # Counting how many times every ball was chosen
  balls_count = as.factor(t(balls_chosen1))
  balls_count = sort(balls_count, decreasing = FALSE)
  balls_count = c(balls_count)
  # Printing out the count
  cat("\014")
  print("- Correct Numbers Counting (without order distinction):")
  print(table(balls_count))
  # Plotting the count
  library(ggplot2)
  qplot(balls_count, 
        geom="histogram", 
        binwidth = 1, 
        colour = I("black"), 
        xlab = "Ball Number", 
        ylab = "Times Raffled",
        main = "- Correct Numbers Counting (without order distinction):")
}
# - Correct Numbers Counting (with order distinction):
{
  # Counting how many times every ball was chosen
  balls_count2 = c()
  i = 1
  for(i in 1:15){
    balls_count2 = cbind(balls_count2, as.matrix(table(balls_chosen[i])))
  }
  colnames(balls_count2) = c("1º", "2º", "3º", "4º", "5º", "6º", "7º", "8º", 
                             "9º", "10º", "11º", "12º", "13º", "14º", "15º")
  # Prints out the count of every taking order
  cat("\014")
  print("- Correct Numbers Counting (with order distinction):")
  balls_count2
}
# - Wrong Numbers Counting (without order distinction):
{
  # Counting how many times every ball was chosen
  balls_wrong_count = as.factor(t(balls_wrong))
  balls_wrong_count = sort(balls_wrong_count, decreasing = FALSE)
  balls_wrong_count = c(balls_wrong_count)
  # Printing out the count
  cat("\014")
  print("- Wrong Numbers Counting (without order distinction):")
  print(table(balls_wrong_count))
  # Plotting the count
  library(ggplot2)
  qplot(balls_wrong_count, 
        geom="histogram", 
        binwidth = 1, 
        colour = I("black"), 
        xlab = "Ball Number", 
        ylab = "Times Raffled",
        main = "- Wrong Numbers Counting (without order distinction):")
}

# -> Data Modeling (Logistic Regression):
{
  # - Data Normalizing:
  Data = random_balls_normalized
  Data[1:15] = scale(Data[1:15])
  # - Train/Test Split:
  set.seed(2)
  testindex = sample.int(nrow(Data))[1:floor(0.1*nrow(Data))]
  TestData = Data[testindex,];
  TrainData = Data[-testindex,]
  # - Modeling:
  model = glm(Winner~.,family=binomial(link='logit'),data=TrainData, control = list(maxit = 50))
  # - Prediction:
  fitted.results = predict(model,newdata=TestData,type='response')
  yhat = ifelse(fitted.results > 0.5,1,0) # Predicted Value
  result = TestData[,c('Winner')]         # Actual Value
  # - Evaluation:
  ConfutationMatrix = base::table(yhat, result)
  accuracy1 = round(100*mean(yhat==result), 2)
  # - Plotting Confusion Matrix
  library(ggplot2)
  confusion_matrix2 = as.data.frame(ConfutationMatrix)
  # - Saves the confusion matrix as an image
  {
    png(file="Logistic Regression - Confusion Matrix.png", width=500, height=500)
    ggplot(data = confusion_matrix2,mapping = aes(x = result, y = yhat)) +
      geom_tile(aes(fill = Freq)) + geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "brown", high = "green") + 
      ggtitle("- Confusion Matrix (Logistic Regression): ") +
      guides(fill = FALSE) + labs(x="", y="", subtitle=paste("Accuracy: ", accuracy1, "%"))
    dev.off()
  }
}
# -> Data Modeling (Decision Tree):
{
  # - Data Normalizing: Turns numerical to categorical in order to a better decision tree plot
  Data2 = random_balls_normalized
  Data2$Winner[Data2$Winner==1]="Win"
  Data2$Winner[Data2$Winner==0]="Lose"
  # - Train/Test Split:
  library(rpart)
  library(rpart.plot)
  n = nrow(Data2)
  train_ind = sample(c(1:n), size = 10)
  smp_size = floor(0.75 * n) # 75% of the sample size
  set.seed(123)
  train_ind = base::sample(c(1:n), size = smp_size)
  Data2_train = Data2[train_ind, ]
  Data2_test = Data2[-train_ind, ]
  # - Modeling:
  newDT = rpart(Winner ~ ., Data2_train, method = "class")
  result2 = predict(newDT, Data2_test, type = "class")
  # - Plotting and Saving the Tree:
  {
    png(file="Decision Tree - Tree Plot.png", width=450, height=450)
    rpart.plot(newDT, type = 1, extra = 2, under = TRUE, faclen=10, cex = .8)
    dev.off()
  }
  # - Evaluation:
  ConfusionMatrix = base::table(Data2_test$Winner, result2)
  accuracy2 = round(100*mean(Data2_test$Winner==result2), 2)
  # - Plotting Confusion Matrix
  library(ggplot2)
  confusion_matrix = as.data.frame(ConfusionMatrix)
  # - Saves the confusion matrix as image
  {
    png(file="Decision Tree - Confusion Matrix.png", width=500, height=500)
    ggplot(data = confusion_matrix,mapping = aes(x = result2, y = Var1)) +
      geom_tile(aes(fill = Freq)) + geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "brown", high = "green") + 
      ggtitle("- Confusion Matrix (Decision Tree):") +
      guides(fill = FALSE) + labs(x="", y="", subtitle=paste("Accuracy: ", accuracy2, "%"))
    dev.off()
  }
}
