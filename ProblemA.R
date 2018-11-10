# Load libraries and data
library(rectools)
library(recosystem)
load('ML100K')

# Set up data by naming columns and then merging data sets
names(udata) <- c('usernum', 'movienum', 'rating', 'transID')
set.seed(9999)
testidxs <- sample(1:nrow(udata),1000)
testset <- udata[testidxs,1:3]
trainset <- udata[-testidxs,1:3]

## Function for calculating MAPE error
MAPE = function(df_list) {
  inner_sum <- sum(abs(df_list[["rating"]] - df_list[["prediction"]])/abs(df_list[['rating']]), na.rm=T)
  error = inner_sum/nrow(df_list)
  return (round(error, 3))
}

get_MAPE_errors_from_dataset <- function(trainingset, testset) {
  # Initiliaze resulting column vectors
  k_column <- c()
  mape_column <- c()
  
  # For each rank n, train data set and cross validate, using MAPE error as a benchmark
  lapply(seq(5, 200, 5), function(k) {
    # Train recommender system using matrix vectorization
    recoObject <- trainReco(ratingsIn = trainset, rnk = k, nmf = TRUE)
    
    # Get predictions
    prediction <- predict.RecoS3(recoObj = recoObject, testSet = testset)
    
    # Get MAPE error
    prediction_with_rating <- cbind(testset, prediction)
    prediction_MAPE_error <- MAPE(prediction_with_rating)
    
    # Append results to result
    k_column <<- c(k_column, k)
    mape_column <<- c(mape_column, prediction_MAPE_error)
  })
  result <- cbind(k_column, mape_column)
}

# Set prediction tables
prediction_testset_with_mape_table <- get_MAPE_errors_from_dataset(trainset, testset)
prediction_trainset_with_mape_table <- get_MAPE_errors_from_dataset(trainset, trainset)

# Convert prediction tables into data frames
df_prediction_testset_with_mape_table <- as.data.frame(prediction_testset_with_mape_table)
df_prediction_trainset_with_mape_table <- as.data.frame(prediction_trainset_with_mape_table)

# write.csv(df_prediction_testset_with_mape_table, "prediction_of_testset.csv")
# write.csv(df_prediction_trainset_with_mape_table, "prediction_of_trainset.csv")

library(ggplot2)
# Plot the graphs: Line Graph
# https://stackoverflow.com/questions/21192002/how-to-combine-2-plots-ggplot-into-one-plot#21192612
p <- ggplot() +
  # blue plot
  geom_point(data=df_prediction_testset_with_mape_table, aes(x=k_column, y=mape_column)) + 
  geom_smooth(data=df_prediction_testset_with_mape_table, aes(x=k_column, y=mape_column), fill="blue",
              colour="darkblue", size=1) +
  # red plot
  geom_point(data=df_prediction_trainset_with_mape_table, aes(x=k_column, y=mape_column)) + 
  geom_smooth(data=df_prediction_trainset_with_mape_table, aes(x=k_column, y=mape_column), fill="red",
              colour="red", size=1)
  # Set legend and title

p <- p + ggtitle("Rank vs MAPE for NMF") + xlab("RANK") + ylab("MAPE")
