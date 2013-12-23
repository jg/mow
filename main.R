library(tm)
library(tm.plugin.mail)
library(Snowball)
library(SnowballC)
library(hash)
library(e1071)
library(FSelector)
options(width=140)

prepCorpus <- function(dirname) {
  removeStopWords <- function (doc) {
    return(removeWords(doc, stopwords("english")))
  }
  stemDoc <- function (doc) {
    return(stemDocument(doc, language = "english"))
  }
  convert <- function (t) {
    return(iconv(t, "ISO-8859-1", "UTF-8"))
  }
  corpus <- Corpus( DirSource (dirname), readerControl=list(reader= readMail, language='en_US' ) )
  data <- corpus
  data <- tm_map(data, convert)
  data <- tm_map(data, removeSignature)
  data <- tm_map(data, as.PlainTextDocument)
  data <- tm_map(data, tolower)
  data <- tm_map(data, removeStopWords)
  data <- tm_map(data, stripWhitespace)
  data <- tm_map(data, removeNumbers)
  data <- tm_map(data, removePunctuation)
  data <- tm_map(data, stemDoc)
  return(data)
}

kFoldIndices <- function(Nobs,K=5) {
  rs <- runif(Nobs)
  id <- seq(Nobs)[order(rs)]
  k <- as.integer(Nobs*seq(1,K-1)/K)
  k <- matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] <- k[,1]+1
  l <- lapply(seq.int(K),function(x,k,d) 
              list(train=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
                   test=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
}

createMainDocumentTermMatrix <- function (data, attributes) {
  l = list()
  li = 1
  classColumn = vector(mode="character", length=0)

  for (i in 1:length(data)) {
    classColumn = c(classColumn, rep(names(data)[i], dim(data[[i]])[1]))
  }

  l[[li]] = as.factor(classColumn)
  li = li + 1

  for (i in 1:length(attributes)) {
    attributeVector = vector(mode="numeric", length=0)
    for (j in 1:length(data)) {
      attribute = attributes[i]
      dtm = as.matrix(data[[j]])
      rowTerms = dimnames(dtm)[[2]]
      if (attribute %in% rowTerms) {
        attributeVector = c(attributeVector, dtm[,attribute])
      } else {
        attributeVector = c(attributeVector, rep(0, dim(dtm)[1]))
      }
    }
    l[[li]] = attributeVector
    li = li + 1
  }

  df = do.call(cbind.data.frame, l)
  names(df) = c("class", attributes)

  return(df)
}

createDocumentTermMatrix <- function (corpus, maxAllowedSparseness) {
  dtm <- removeSparseTerms(DocumentTermMatrix(corpus), maxAllowedSparseness)
  # get rid of row names
  rownames(dtm) = c()
  return (dtm)
}

err <- function (y.true, y.pred) {
  sum(y.pred!=y.true)/length(y.true)
}

prepCorpora <- function (dirs) {
  corpora <- lapply(dirs, function (dirName) {
    return(prepCorpus(dirName))
  })
  names(corpora) <- dirs
  return(corpora)
}

prepData <- function (corpora, maxAllowedSparseness) {
  data <- lapply(corpora, function (corpus) {
    return(createDocumentTermMatrix(corpus, maxAllowedSparseness))
  })
  names(data) <- names(corpora)

  return(data)
}

getAttributes <- function (data) {
  attributes <- unique(unlist(lapply(data, function (dtm) {
    return(dimnames(dtm)[[2]])
  })))
  names(attributes) <- c()
  return(attributes)
}

createMainDTM <- function(corpora, threshold) {
  print(paste("Running threshold: ", threshold))
  dtms <- prepData(corpora, threshold)
  attributes <- getAttributes(dtms)
  print(paste("Attribute count: ", length(attributes)))
  print(paste("Attributes: ", attributes))

  df = createMainDocumentTermMatrix(dtms, attributes)
  return(df)
}

meanClassificationError <- function (corpora, threshold, k) {
  df = createMainDTM(corpora, threshold)

  k = 2
  folds = kFoldIndices(dim(df)[1], k)
  errors = unlist(lapply(folds, function (fold) {
    train = df[fold$train,]
    test = df[fold$test,]
    model <- naiveBayes(class ~ ., data=train, laplace = laplace)
    return(err(test$class, predict(model, test[,-1])))
  }))
  print(paste(k, "-fold cross validation mean error: ", mean(errors)))
  return (mean(errors))
}



# dirs <- c('alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware', 'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles', 'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space', 'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc', 'talk.religion.misc')
dirs <- c('alt.atheism', 'comp.graphics', 'talk.politics.guns')

m = createMainDTM(corpora, 0.8)

# dirs <- c('alt.atheism', 'comp.graphics')

# corpora <- prepCorpora(dirs)
# maxSparsenessThresholds <- list(0.7, 0.6, 0.5, 0.4)
# meanClassificationError(corpora, 0.7, 2)
# results = lapply(maxSparsenessThresholds, function(threshold) {
  # return(meanClassificationError(corpora, threshold, 3)
# })

# model$apriori
# model$tables
# predict(model, hv.test[,-1], type="raw")
