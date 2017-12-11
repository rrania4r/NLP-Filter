
# This file will be used to generate training sentences for the speech filter.
# Eventually, may want to turn this into a apckage function.

##### CREATE EXTRA SENTENCES FROM ALGO TRAINING ##### -----------------------------------

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
  
  sentences <- plyr::mdply(partsDF, paste, .expand = FALSE)[[2]] 
  
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
               {llply(c("N", "M", "F"), make_Sentences_Template2, category = commentType) %>%              ## use Template 2
                   c(recursive = TRUE)},
               {make_Sentences_Template3(category = commentType) %>%                                       ## use Template 1
                   c(recursive = TRUE)},
               {make_Sentences_Template4(category = commentType) %>%                                       ## use Template 1
                   c(recursive = TRUE)}, 
               {make_Sentences_Template5() %>%                                       ## use Template 5
                   c(recursive = TRUE)}
  )
  
  return(DF)
}


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

algoCorpus <- llply(1:5, make_Corpus)
algoCorpus <- c(algoCorpus[[1]], algoCorpus[[2]], 
                algoCorpus[[3]], algoCorpus[[4]], algoCorpus[[5]])


rm(Template1Env, Template2Env, Template3Env, Template4Env, Template5Env)
rm(make_Sentences_Template1, make_Sentences_Template2, make_Sentences_Template3, 
   make_Sentences_Template4, make_Sentences_Template5)
rm(make_Corpus, make_TemplateCorpus)
