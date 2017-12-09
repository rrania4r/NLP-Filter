
# DATE: 09.12.2017
# Trying to add the nuance that ich statements are not offensive.
#
# In this file we will create an elementary German sentence generator in R.
# We will use this to generate a training set for our algorithm,
# which will have to classify text as "Hate Speech", or not.
#

library(tidyverse)
library(plyr)
library(stringr)
library(tm)
library(RWeka)


library(caret)   # For learning algorithm training

library(kernlab) # For SVM
# library(rnn) # For Neural Networks
# library(xgboost)    # For Neural Networks
# library(party) # For cforest
# library(evtree) # For evtree
# library(gbm) # For gbm
# library(deepboost) # For deepboost

library(doParallel) # This, to allow parallel processing
registerDoParallel(cores = 3) # Use three core processors in parallel



#### Template 1: #### -------------------------------------------------------------------

# "Du bist <epithet>"  OR "Sie sind <epithet>,
# where an epithet can be an adjective plus a noun, or just a noun

Template1Env <- new.env(parent = emptyenv())

Template1Env$emphasis <- c("", "ganz", "wirklich", "sehr", "echt", "so")

Template1Env$adjBad <- c("tot", "blöd", "deppert", "scheisse", "saublöd", "saudumm", "ekelig", "ekelhaft")

Template1Env$adjGood <- c("toll", "super", "nett", "schön", "intelligent", "wunderbar", "gut", "schlau", "fabelhaft")

make_Sentences_Template1 <- function(category = "Good") {
  
  emph <- Template1Env$emphasis
  
  adj <- paste0("adj", category) %>%
    get(pos = Template1Env) 
  
  partsDF <- expand.grid(a = c("Du bist", "Sie sind", "Ihr seid"), b = emph, c = adj)
  
  sentences <- mdply(partsDF, paste, .expand = FALSE)[[2]] 
  
  return(sentences) 
}





#### Template 2: #### -------------------------------------------------------------------

# "Du bist <article> <epithet>"

# Good epithets:
# ein <neutral adj/good adj> <good male/neuter noun>
# eine <neutral adj/good adj> <good female noun>

# Bad epithets:
# ein <neutral adj/bad adj> <bad male/neuter noun>
# eine <neutral adj/bad adj> <bad female noun>

# We will use a vector to capture each type of adj/noun, and then use exapnd.grid to get all possible
# combinations.  Then we can paste them all together (or perhaps paste already does that?)

Template2Env <- new.env(parent = emptyenv())

Template2Env$adjGood <- c("gut", "nett", "schön", "wunderbar", "lieb", "freundlich", "toll", "fantastisch", "echt", "geschickt", "klug", "gutmütig")
Template2Env$adjGoodF <- paste(Template2Env$adjGood, "e", sep = "")
Template2Env$adjGoodM <- paste(Template2Env$adjGood, "er", sep = "")
Template2Env$adjGoodN <- paste(Template2Env$adjGood, "es", sep = "")

Template2Env$nounM <- c("Mann", "Junge", "Student", "Professor", "Handwerker", "Vater", "Sohn", "Chef", "Lehrer", "Kollege", "Freund")
Template2Env$nounGoodM <- c(Template2Env$nounM, "Held")
Template2Env$nounF <- c("Frau", "Studentin", "Professorin", "Handwerkerin", "Mutter", "Tochter", "Person", "Chefin", "Lehrerin", "Kollegin", "Freundin")
Template2Env$nounGoodF <- c(Template2Env$nounF, "Heldin") 
Template2Env$nounN <- c("Kind")
Template2Env$nounGoodN <- c(Template2Env$nounN, "Genie", "Wunder", "Wunderkind")

Template2Env$nounBadF <- c("Hure", "Schlampe", "Kuh", "Sau")
Template2Env$nounBadM <- c("Scheisskopf", "Hund", "Schwein", "Idiot", "Arschloch", "Arsch", "Aas", "Trottel", "Affe", "Hurensohn", "Feigling", "Schwachkopf")
Template2Env$nounBadN <- c("Vieh", "Weichei", "Viech", "Luder")

Template2Env$adj <- c("riesig", "gross", "groß", "echt", "wahr")
Template2Env$adjF <- paste(Template2Env$adj, "e", sep = "")
Template2Env$adjM <- paste(Template2Env$adj, "er", sep = "")
Template2Env$adjN <- paste(Template2Env$adj, "es", sep = "")

Template2Env$adjBad <- c("blöd", "dämlich", "hässlich", "deppert", "idiotisch", "stinkig", "faul", "beschissen", "feig")
Template2Env$adjBadF <- paste(Template2Env$adjBad, "e", sep = "")
Template2Env$adjBadM <- paste(Template2Env$adjBad, "er", sep = "")
Template2Env$adjBadN <- paste(Template2Env$adjBad, "es", sep = "")

make_Sentences_Template2 <- function(gender = "N", category = "Good") {
  
  adj <- paste0("adj", category, gender) %>%
    get(pos = Template2Env) 
  
  adj2 <- paste0("adj", gender) %>%
    get(pos = Template2Env) 
  
  noun <- paste0("noun", category, gender) %>%
    get(pos = Template2Env)
  
  noun2 <- paste0("noun", gender) %>%
    get(pos = Template2Env)
  
  if (gender == "F") {
    article <- "eine"
  } else {
    article <- "ein"
  }
  
  partsDF <- expand.grid(a = c("Du bist", "Sie sind"), b = article, c = c("", adj, adj2), d = noun)
  sentences <- mdply(partsDF, paste, .expand = FALSE)[[2]] 
  
  if (category == "Bad") {
    partsDF2 <- expand.grid(a = "Du bist", b = article, c = adj, d = noun2)
    sentences2 <- mdply(partsDF2, paste, .expand = FALSE)[[2]]
    sentences <- c(sentences, sentences2)
  }
  
  return(sentences) 
}



#### Template 3: #### -------------------------------------------------------------------

Template3Env <- new.env(parent = emptyenv())

Template3Env$object <- c("dich", "Sie", "euch")

Template3Env$preverbBad <- c("töte", "erwürge", "vergewaltige", "hasse", "verabscheue")

Template3Env$preverbGood <- c("mag", "liebe", "respektiere", "bewundere", "kenne", "höre", "sehe")


make_Sentences_Template3 <- function(category = "Good") {
  
  obj <- Template3Env$object
  
  preverb <- paste0("preverb", category) %>%
    get(pos = Template3Env) 
  
  partsDF <- expand.grid(a = preverb, b = obj)
  
  sentences <- mdply(partsDF, paste, .expand = FALSE)[[2]] 
  
  return(sentences) 
}


#### Template 4: #### -------------------------------------------------------------------

Template4Env <- new.env(parent = emptyenv())

Template4Env$object <- c("dich", "Sie", "euch")

Template4Env$preverb <- c("will", "werde", "wird", "sollte", "muss", "müsste", "möchte")

Template4Env$postverbGood <- c("beraten", "respektieren", "hören", "sehen", "verstehen", "wiedersehen", "begrüßen", "einladen")

Template4Env$postverbBad <- c("töten", "umbringen", "erschiessen", "erwürgen", "vergewaltigen")


make_Sentences_Template4 <- function(category = "Good") {
  
  obj <- Template4Env$object
  
  preverb <- Template4Env$preverb
  
  postverb <- paste0("postverb", category) %>%
    get(pos = Template4Env) 
  
  partsDF <- expand.grid(a = preverb, b = obj, c = postverb)
  
  sentences <- mdply(partsDF, paste, .expand = FALSE)[[2]] 
  
  return(sentences) 
}




#### Template 5: #### -------------------------------------------------------------------

# "Ich bin <epithet>", "Wir sind <epithet>"
# where an epithet can be an adjective plus a noun, or just a noun

Template5Env <- new.env(parent = emptyenv())

Template5Env$emphasis <- c("", "ganz", "wirklich", "sehr", "echt", "so")

Template5Env$adjBad <- c("tot", "blöd", "deppert", "scheisse", "saublöd", "saudumm", "ekelig", "ekelhaft")

Template5Env$adjGood <- c("toll", "super", "nett", "schön", 
                          "intelligent", "wunderbar", "gut", 
                          "schlau", "fabelhaft", Template5Env$adjBad)
# For this template, all adjectives are "good"

make_Sentences_Template5 <- function(category = "Good") {
  
  emph <- Template5Env$emphasis
  
  adj <- paste0("adj", category) %>%
    get(pos = Template5Env) 
  
  partsDF <- expand.grid(a = c("Ich bin", "Wir sind"), b = emph, c = adj)
  
  sentences <- mdply(partsDF, paste, .expand = FALSE)[[2]] 
  
  return(sentences) 
}



#### Make Corpus Function: #### ---------------------------------------------------------

make_TemplateCorpus <- function(commentType, template = 1){
  DF <- switch(template,
               {make_Sentences_Template1(category = commentType) %>%                                       ## use Template 1
                   c(recursive = TRUE)},
               {DFGood <- llply(c("N", "M", "F"), make_Sentences_Template2, category = commentType) %>%    ## use Template 2
                 c(recursive = TRUE)},
               {make_Sentences_Template3(category = commentType) %>%                                       ## use Template 1
                   c(recursive = TRUE)},
               {make_Sentences_Template4(category = commentType) %>%                                       ## use Template 1
                   c(recursive = TRUE)}, 
               {make_Sentences_Template5() %>%                                       ## use Template 5
                   c(recursive = TRUE)}
  )
  print(commentType)
  comments <- VectorSource(DF)
  corpusComments <- VCorpus(comments, readerControl = list(language = "ger")) %>%
    tm_map(stripWhitespace)
  meta(corpusComments, "Classification") <- commentType
  
  return(corpusComments)
}


#### RUN #### : -------------------------------------------------------------------------

make_Corpus <- function(i) {
  if (i == 5) {
    DF <- make_TemplateCorpus("Good", template = 5) 
  } else {
    DF <- llply(c("Good", "Bad"), make_TemplateCorpus, template = i) 
    DF <- c(DF[[1]], DF[[2]])
  }

  names(DF) <- paste("T", i, "_", 1:length(DF), sep = "")
  return(DF)
}

fullCorpus <- llply(1:5, make_Corpus)
fullCorpus <- c(fullCorpus[[1]], fullCorpus[[2]], 
                fullCorpus[[3]], fullCorpus[[4]], fullCorpus[[5]])

meta(fullCorpus)

### The NGramTokenizer comes with default min 1-gram, max 3-gram.  if I want to change that,
### I need to create a new function for use with DocumentTermMatrix

myTokenizer <- function(c) {
  NGramTokenizer(x = c, control = Weka_control(max = 6))
}

dtm <- DocumentTermMatrix(fullCorpus, control = list(tokenize = myTokenizer, wordLengths = c(2, Inf)))

inspect(dtm[500:510, 2480:2484])
dim(dtm)
length(fullCorpus)
row.names(dtm)
names(fullCorpus)
all(names(fullCorpus) == row.names(dtm))

length(unique(row.names(dtm)))
length(names(fullCorpus))
length(unique(names(fullCorpus)))


#### Learning #### ----------------------------------------------------------------------

## Using caret:

numFolds <- 5
numRepeats <- 5
tuneLen <- 5

X <- as.matrix(dtm)
Y <- meta(fullCorpus)
Y$Name <- names(fullCorpus)
all(Y$Name == row.names(X))

## We will randomly rearrange the rows of X (and correspondingly Y), because ------------
## they are currently too neatly sorted into good/bad
set.seed(390)
tempN <- nrow(X)
new <- sample(1:tempN, size = tempN, replace = FALSE)

X <- X[new, ]
Y <- Y[new, ]

all(row.names(X) == Y$Name)

rm(tempN, new)

## Now split data into test and training ------------------------------------------------
set.seed(72)
inTrain <- createDataPartition(y = Y$Classification, p = 0.8, list = FALSE)

TrainX <- X[inTrain, ]
TrainY <- Y[inTrain, ]

TestX <- X[-inTrain, ]
TestY <- Y[-inTrain, ]

trainIndex <- createMultiFolds(TrainY$Classification, k = numFolds, times = numRepeats)

fitControl <- trainControl(## 5-fold CV repeated 5 times
  method = "repeatedcv",
  number = numFolds,
  repeats = numRepeats,
  classProbs = TRUE,     # so that the model will generate class probabilities, not only the predicted class.
  index = trainIndex,
  allowParallel = TRUE)



## SVMLinear: -----------------------------------------------------------------------

set.seed(8)

# preProc <- c("center", "scale")

SVMLinearfit <- train(x = TrainX,
                      y = TrainY$Classification,
                      method = 'svmLinear',
                      #                    preProcess = preProc,
                      trControl = fitControl,
                      tuneLength = tuneLen) 

SVMLinearfit
summary(SVMLinearfit)

predictions <- predict(SVMLinearfit, newdata = TestX)
confusionMatrix(predictions, TestY$Classification)
# Awesome 99.4% on test, 99.6% on Training!
# Performs better without preprocessing!
# Now we have 99% accuracy on training and test sets.
# BUT, we have duplicate row names.  This needs to be checked!
predictions <- predict(SVMLinearfit, newdata = TestX, type = "prob")
predictions[1:10, ]

which(predictions != TestY$Classification)
error <- TestX[predictions != TestY$Classification, ]
error

## evtree: ------------------------------------------------------------------------------

set.seed(8)

# preProc <- c("center", "scale")

evtreefit <- train(x = TrainX,
                      y = TrainY$Classification,
                      method = 'evtree',
                      #                    preProcess = preProc,
                      trControl = fitControl,
                      tuneLength = tuneLen) 
evtreefit
summary(evtreefit)

predictions <- predict(evtreefit, newdata = TestX)
confusionMatrix(predictions, TestY$Classification)

predictions <- predict(evtreefit, newdata = TestX, type = "prob")
predictions[1:10, ]


## gbm: ---------------------------------------------------------------------------------

set.seed(8)

# preProc <- c("center", "scale")

gbmfit <- train(x = TrainX,
                   y = TrainY$Classification,
                   method = 'gbm',
                   trControl = fitControl,
                   tuneLength = tuneLen) 
gbmfit
summary(gbmfit)

predictions <- predict(gbmfit, newdata = TestX)
confusionMatrix(predictions, TestY$Classification)

predictions <- predict(gbmfit, newdata = TestX, type = "prob")
predictions[1:10, ]


## cforest: -----------------------------------------------------------------------------

# Slow, and caused computer to crash
set.seed(8)

# preProc <- c("center", "scale")

cforestfit <- train(x = TrainX,
                y = TrainY$Classification,
                method = 'cforest',
                trControl = fitControl,
                tuneLength = tuneLen) 
cforestfit
summary(cforestfit)

predictions <- predict(cforestfit, newdata = TestX)
confusionMatrix(predictions, TestY$Classification)

predictions <- predict(cforestfit, newdata = TestX, type = "prob")
predictions[1:10, ]


## deepboost: ---------------------------------------------------------------------------
# caused the computer to crash.
# used too much memory!
set.seed(8)

# preProc <- c("center", "scale")

deepboostfit <- train(x = TrainX,
                    y = TrainY$Classification,
                    method = 'cforest',
                    trControl = fitControl,
                    tuneLength = tuneLen) 
deepboostfit
summary(cforestfit)

predictions <- predict(cforestfit, newdata = TestX)
confusionMatrix(predictions, TestY$Classification)

predictions <- predict(cforestfit, newdata = TestX, type = "prob")
predictions[1:10, ]


## xgBoost Linear: ----------------------------------------------------------------------

## This is too slow!
set.seed(8)

# preProc <- c("center", "scale")

xgBoostLinfit <- train(x = TrainX,
                      y = TrainY$Classification,
                      method = 'xgbLinear',
                      #                    preProcess = preProc,
                      trControl = fitControl,
                      tuneLength = tuneLen)


xgBoostLinfit
summary(xgBoostLinfit)

predictions <- predict(xgBoostLinfit, newdata = TestX)
confusionMatrix(predictions, TestY$Classification)
# Awesome 100% on test, 98% on Training!
# Performs better without preprocessing!
# Now we have 99% accuracy on training and test sets.
# BUT, we have duplicate row names.  This needs to be checked!
predictions <- predict(xgBoostLinfit, newdata = TestX, type = "prob")
predictions[1:10, ]


## xgBoost Tree: ------------------------------------------------------------------------


set.seed(8)

# preProc <- c("center", "scale")

xgBoostTreefit <- train(x = TrainX,
                       y = TrainY$Classification,
                       method = 'xgbTree',
                       #                    preProcess = preProc,
                       trControl = fitControl,
                       tuneLength = tuneLen)


xgBoostTreefit
summary(xgBoostTreefit)

predictions <- predict(xgBoostTreefit, newdata = TestX)
confusionMatrix(predictions, TestY$Classification)
# Awesome 100% on test, 98% on Training!
# Performs better without preprocessing!
# Now we have 99% accuracy on training and test sets.
# BUT, we have duplicate row names.  This needs to be checked!
predictions <- predict(xgBoostTreefit, newdata = TestX, type = "prob")
predictions[1:10, ]


## Create Final Prediction Algorithm: ---------------------------------------------------

FinalX <- rbind(TrainX, TestX)
FinalY <- rbind(TrainY, TestY)
all(row.names(FinalX) == FinalY$Name)

svmTuneGrid <- expand.grid(SVMLinearfit$bestTune)

FinalFitControl <- trainControl(method = "none")

set.seed(1001)

svmModel <- train(x = FinalX,
                  y = FinalY$Classification,
                  method = "svmLinear",
                  trControl = FinalFitControl,
                  tuneGrid = svmTuneGrid,
                  prob.model = TRUE)

svmModel

summary(svmModel)

predictions <- predict(svmModel, newdata = FinalX)
confusionMatrix(predictions, FinalY$Classification)
# This final model gets it 100% right

predictions <- predict(svmModel, newdata = FinalX, type = "prob")
predictions[1:10, ]
save(svmModel, file = "./Algo_with_Probs.RData")

