#   This is a machine learning made in order to build a model of lottery good numbers.
#   The lottery we'll make the model is called "Lotofacil", it basically work with a choosing
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
    balls_wrong = c()
    total_balls = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",  "13", "14", 
                    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")
    for(i in nrow(balls_chosen):1){
    balls_wrong = rbind(setdiff(total_balls, balls_chosen_sorted[i,]), balls_wrong)
    }
  }
  # Filling in the variable of wrong numbers just to get the same shape of right numbers
  {
    balls_wrong_complete = balls_wrong
    for(i in 1:5){
          balls_wrong_complete = cbind(balls_wrong_complete, " ")
        }
    for(i in 11:15){
        for(j in 1:nrow(balls_chosen)){
          r = sample(1:10, 1)
          balls_wrong_complete[j,i] = setdiff(total_balls, balls_wrong_complete[j,])[r]
          # Checking if there's no NA value on the variable
          sum(is.na(balls_wrong_complete))
        }
    }
  }
  # Makes a column responsible for classifing if someone won or not
  {
    balls_wrong_complete = cbind(balls_wrong_complete, "0")
    colnames(balls_wrong_complete) = c("[,1]", "[,2]", "[,3]", "[,4]", "[,5]", "[,6]", "[,7]", 
                                       "[,8]", "[,9]", "[,10]", "[,11]", "[,12]", "[,13]", "[,14]",
                                       "[,15]", "Winner")
    balls_chosen = cbind(balls_chosen, "1")
    colnames(balls_chosen) = c("[,1]", "[,2]", "[,3]", "[,4]", "[,5]", "[,6]", "[,7]", 
                                       "[,8]", "[,9]", "[,10]", "[,11]", "[,12]", "[,13]", "[,14]",
                                       "[,15]", "Winner")
  }
  # That's the final variable (after normalizing process) we'll use to make the model
  balls_normalized = rbind(balls_chosen, balls_wrong_complete)
  # Randomly sampling the dataset to help later on the modeling
  random_balls_normalized = balls_normalized[sample.int(nrow(balls_normalized))[1:nrow(balls_normalized)],]
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
