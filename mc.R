# Load the libraries
library(sparklyr) 
library(dplyr)

# Spark Connection
sc <- spark_connect(master="local[*]")

# Load the ratings file from the hdfs into a SPARK DATAFRAME
# R Documentation - ?spark_read_csv spark_read_csv {sparklyr}	- Read a CSV file into a Spark DataFrame
ratings <- spark_read_csv(sc, "mysampleread", "hdfs://localhost:9000/user/hduser/sample/ratings.csv", header = TRUE, columns = NULL,
                          infer_schema = TRUE, delimiter = ",",charset = "UTF-8", null_value = NULL, options = list(),
                          repartition = 0, memory = TRUE, overwrite = TRUE)
head(ratings)

# Model using ALS    
model <- ml_als_factorization(ratings, rating.column = "rating", user.column = "userId",
                     item.column = "movieId", rank = 10L, regularization.parameter = 0.1,
                     implicit.preferences = FALSE, alpha = 1, nonnegative = FALSE,
                     iter.max = 10L, ml.options = ml_options())
summary(model)

# Predictions
predictions <- model$.model %>%
  invoke("transform", spark_dataframe(ratings)) %>%
  collect()

head(predictions)

# Close Spark Connection
sc <-spark_disconnect_all()
