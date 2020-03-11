

m.assess <- function(df, mod1, y){
  # df is the whole dataframe; mod1 is the lm; y is the objective variable of the lm
  train <- df[1:(length(df$y)/2)]
  test <- mtcars[(length(df$y)/2):length(df$y),]
  prediction <- predict(mod1, test)
  rmse.v <- rmse(test$y, prediction)
  mae.v <- mae(test$y, prediction)
  pbias.v <- pbias(test$y, prediction)
  names <- c("RMSE", "MAE", "PBIAS")
  values <- c(rmse.v, mae.v, pbias.v)
  tribble <- rbind(names, values)
  return(tribble)
}


