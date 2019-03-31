setwd("C:/Users/Meyerhofer/Desktop/SMDELabs/SMDE-2/dataset/")


getDataMarathon <- function(){
  #We read all datasets we have in our folder
  files <- list.files(pattern="*.csv")
  marathonData <- lapply(files, read.csv)
  
  #we normalize the data columns and names
  marathonData[[1]]$X <- NULL
  marathonData[[3]]$X <- NULL
  names(marathonData[[2]])[names(marathonData[[2]]) == "X"] <- "X.1"
  
  #Meteorogical conditions on the day of the race
  marathonData[[1]]$Clima <- as.factor('Rain')
  marathonData[[2]]$Clima <- as.factor('Sun')
  marathonData[[3]]$Clima <- as.factor('Sun')

  #We join them in a single dataframe
  marathonData <- do.call("rbind", marathonData) 
  
  #we pre-process this data as follows:
  
  #first we sort the data by finishing time, but prior to that we convert it.
  library(lubridate)
  marathonData$Official.Time <- as.numeric(hms(marathonData$Official.Time))
  sortedIndexes <- sort.int(marathonData$Official.Time, index.return = T)$ix
  marathonData <- marathonData[sortedIndexes, ]
  rownames(marathonData) <- 1:nrow(marathonData)
  

  #Converting time variables
  marathonData$X5K <- as.numeric(hms(marathonData$X5K))
  marathonData$X10K <- as.numeric(hms(marathonData$X10K))
  marathonData$X15K <- as.numeric(hms(marathonData$X15K))
  marathonData$X20K <- as.numeric(hms(marathonData$X20K))
  marathonData$Half <- as.numeric(hms(marathonData$Half))
  marathonData$X25K <- as.numeric(hms(marathonData$X25K))
  marathonData$X30K <- as.numeric(hms(marathonData$X30K))
  marathonData$X35K <- as.numeric(hms(marathonData$X35K))
  marathonData$X40K <- as.numeric(hms(marathonData$X40K))
  marathonData$Proj.Time <- as.numeric(hms(marathonData$Proj.Time))
  marathonData$Pace <- as.numeric(hms(marathonData$Pace))
  
  #Is it from Boston?
  marathonData$Local <- as.factor(ifelse(marathonData$City == "Boston", "Boston", "NoBoston"))
  #Is it from USA?
  marathonData$LocalCountry <-  as.factor(ifelse(marathonData$Country == "USA", "USA", "NoUsa"))
  
  #Is it from Africa?
  africa <- c('ALG', 'BDI', 'EGY', 
              'ETH','JOR' ,'KEN',
              'KUW', 'MAR', 'MAS',
              'NGR', 'OMA', 'QAT', 
              'UGA', 'ZIM')
  
  marathonData$African <- as.factor(ifelse(marathonData$Country %in% africa, "Africa", "NoAfrica"))
  
  #classification by age
  minAge <- min(marathonData$Age)
  maxAge <- max(marathonData$Age)
  ages <- seq(from=minAge, to=maxAge, by=10)
  marathonData$Age <-  cut(marathonData$Age, ages)
  
  #Classification by paces
  paces <- seq(from=min(marathonData$Pace), to=max(marathonData$Pace), by=30)
  marathonData$Pace <- cut(marathonData$Pace, paces)
  
  #Discard variables we are not going to use:
  marathonData$Gender <- NULL
  marathonData$Overall <- NULL
  marathonData$Division <- NULL
  marathonData$Proj.Time  <- NULL
  
  na.omit(marathonData)

}

createModel <- function(dataFromMarathon){
  list(predX5=lm(X5K ~ Age + M.F + Clima + Local 
                 + African, data=dataFromMarathon),
       predX10=lm(X10K ~ Age + M.F + Clima + Local 
                  + African +  X5K, data=dataFromMarathon),
       predX15=lm(X15K ~ Age + M.F + Clima + Local 
                  + African + X5K + X10K, data=dataFromMarathon),
       predX20=lm(X20K ~ Age + M.F + Clima + Local 
                  + African + X5K + X10K + X15K, data=dataFromMarathon),
       predXHalf=lm(Half ~ Age + M.F + Clima + Local 
                    + African + X5K + X10K + X15K + X20K, data=dataFromMarathon),
       predX25=lm(X25K ~ Age + M.F + Clima + Local 
                 + African + X5K + X10K + X15K + X20K + Half, data=dataFromMarathon),
       predX30=lm(X30K ~ Age + M.F + Clima + Local 
                 + African + X5K + X10K + X15K + X20K + Half + X25K, data=dataFromMarathon),
       predX35=lm(X35K ~ Age + M.F + Clima + Local 
                  + African + X5K + X10K + X15K + X20K + Half + X25K + X30K, data=dataFromMarathon),
       predX40=lm(X40K ~ Age + M.F + Clima + Local 
                  + African + X5K + X10K + X15K + X20K + Half + X25K + X30K + X35K, data=dataFromMarathon),
       predFTime=lm(Official.Time ~ Age + M.F + Clima + Local 
       + African + X5K + X10K + X15K + X20K + Half + X25K + X30K + X35K +X40K, data=dataFromMarathon)
  )
} 


simulateExp <- function(model, age, sex, clima, fromBoston, fromAfrica, times, alpha, h2){
  sims <- replicate(times, {
    runner <- data.frame(Age=age, M.F=sex, Clima=clima, Local=fromBoston,  African=fromAfrica)
    
    runner$X5K  <- predict(model$predX5, runner)
    runner$X10K <- predict(model$predX10, runner)
    runner$X15K <- predict(model$predX15, runner)
    runner$X20K <- predict(model$predX20, runner)
    runner$Half <- predict(model$predXHalf, runner)
    runner$X25K <- predict(model$predX25, runner)
    runner$X30K <- predict(model$predX30, runner)
    runner$X35K <- predict(model$predX35, runner)
    runner$X40K <- predict(model$predX40, runner)
    
    t <- as.integer(predict(model$predFTime, runner) + rnorm(1, sd=60))
  })
  
  h <- dt(1-alpha/2, times-1)*sd(sims)/sqrt(times)
  n2 <- ceiling(times*(h/h2)^2)
  
  if (n2 - n >= 1) {
    sims2 <- replicate(max(0, n2 - times), {
      runner <- data.frame(Age=age, M.F=sex, Clima=clima, Local=fromBoston, African=fromAfrica)
      runner$X5K  <- predict(model$predX5, runner)
      runner$X10K <- predict(model$predX10, runner)
      runner$X15K <- predict(model$predX15, runner)
      runner$X20K <- predict(model$predX20, runner)
      runner$Half <- predict(model$predXHalf, runner)
      runner$X25K <- predict(model$predX25, runner)
      runner$X30K <- predict(model$predX30, runner)
      runner$X35K <- predict(model$predX35, runner)
      runner$X40K <- predict(model$predX40, runner)
      t <- as.integer(predict(model$predFTime, runner) + rnorm(1, sd=60))
    })
    sims <- c(sims, sims2)
    
  }
  as.integer(mean(sims))
}



main <- function(){
  dataMarathon <- getDataMarathon()
  model <- createModel(dataMarathon)
  
  
  #validations of the model
  #•library("lmtest")
  #dwtest(model$predFTime, alternative ="two.sided")
  #shapiro.test(residuals(AnovaModel.1))
  #lmtest::bptest(model$predFTime)
  #summary(model$predFTime)
  
  #experimentX(model, n, alpha, h2)
  n <- 30
  alpha <- 0.05
  h2 <- 0.5
  #binary exp
  experiment1(model, n, alpha, h2)
  #age
  experiment2(model, n, alpha, h2)

}


experiment1 <- function(model,n, alpha, h2) {
  #we fix age because we cannot select from more than the two valus. next exp, will decide which is the best age.
  range <- c(-1, 1)
  Age <- c('(18,28]', '(68,78]')
  Gender <- c('M', 'F')
  Weather <- c('Sun', 'Rain')
  fromBoston <-  c('Boston', 'NoBoston')  
  fromAfrica <-  c('Africa', 'NoAfrica') 
  
  S <- lapply(range, function(A) {
    lapply(range, function(G) {
      lapply(range, function(AF) {
        lapply(range, function(B) {
          lapply(range, function(W) {
           # data.frame(Age=age, M.F=sex, Clima=clima, Local=fromBoston,  African=fromAfrica)
            s <- simulateExp(model,Age[A], Gender[G],Weather[W], fromBoston[B] ,fromAfrica[AF], n, alpha, h2)
            names(s) <- paste0(ifelse(A == 1, '+', '-'),
                               ifelse(G == 1, '+', '-'),
                               ifelse(AF == 1, '+', '-'),
                               ifelse(B == 1, '+', '-'),
                               ifelse(W == 1, '+', '-'))
            s
          })
        })
      })
    })
  })
  S <- c(S, recursive=TRUE)
  #df <- data.frame(Seconds=S)
  
  write.csv2(df, 'exp1.csv')
}

experiment2 <- function(model, n, alpha, h2) {
  #In this case we are just iterating throught the only attribute we don't know which is the best yet htat is the age so we fix all the others.
  
  Age <- c('(18,28]', '(28,38]', '(38,48]', '(48,58]', '(58,68]')
  # data.frame(Age=age, M.F=sex, Clima=clima, Local=fromBoston,  African=fromAfrica)
  S <- lapply(Age, function(A) {
    s <- simulateExp(model,A, 'M','Sun', 'NoBoston','NoAfrica',n, alpha, h2)
    names(s) <- A
    s
  })
  S <- c(S, recursive=TRUE)
  df <- data.frame(Seconds=S)
  
  #write.csv2(df, 'exp2.csv')
}
