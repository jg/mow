# main.R
library(tm)
library(tm.plugin.mail)
library(Snowball)
library(SnowballC)
library(hash)
library(e1071)
library(FSelector)

options(width=140)
options(error=traceback)

prepCorpus <- function (dirname) {
  # Reads in a newsgroup directory, loads it into a Corpus, cleans the data
  #
  # Args:
  #   dirname: Directory name (string)
  # Returns:
  #   The Corpus
  corpus <- Corpus( DirSource (dirname), readerControl=list(reader= readMail, language='en_US' ) )
  names(corpus) <- dirname
  cleanedCorpus <- cleanCorpus(corpus)
  return (cleanedCorpus)
}

cleanCorpus <- function (corpus) {
  # Cleans data, returns cleaned Corpus
  removeStopWords <- function (doc) {
    return(removeWords(doc, stopwords("english")))
  }
  stemDoc <- function (doc) {
    return(stemDocument(doc, language = "english"))
  }
  convert <- function (t) {
    return(iconv(t, "ISO-8859-1", "UTF-8"))
  }
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
  return (data)
}

kFoldIndices <- function(Nobs,K=5) {
  # Computes array indices that can be used for k-fold cross validation
  #
  # Args:
  #  Nobs: Number of rows in the array
  #  K: Number of folds
  #
  # Returns:
  #  A list of k elements, each of which has a $train and a $test component.
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

selectDtmColumns <- function (dtm, filterFn) {
  # Selects columns from the documentTermMatrix matching filterFn on
  # term colnames
  #
  # Args:
  #  dtm: documentTermMatrix
  #
  # Returns:
  #  DocumentTermMatrix with non-matching columns removed
  return (dtm[,!(sapply(dimnames(dtm)$Terms, filterFn))])
}

selectDfColumns <- function (df, selectFn) {
  # Selects columns from the dataframe matching filterFn on colnames
  #
  # Args:
  #  df: dataFrame
  #
  # Returns:
  #  dataFrame with matching columns retained
  return (df[,(sapply(dimnames(df)[[2]], selectFn))])
}

createDocumentTermMatrix <- function (corpus, maxAllowedSparseness) {
  # Creates a DocumentTermMatrix from corpus with given maximal sparseness
  dtm <- removeSparseTerms(DocumentTermMatrix(corpus), maxAllowedSparseness)
  # get rid of row names
  rownames(dtm) = c()
  return (dtm)
}

err <- function (y.true, y.pred) {
  sum(y.pred!=y.true)/length(y.true)
}

prepCorpora <- function (dirs) {
  # Runs prepCorpus for each of the directory names in dirs
  #
  # Args:
  #   dirs: list of directory names
  #
  # Returns:
  #   The list of Corpora
  corpora <- lapply(dirs, function (dirName) {
    return(prepCorpus(dirName))
  })
  names(corpora) = dirs
  return(corpora)
}

createDtm <- function (corpus, maxSparseness) {
  # Create a DocumentTermMatrices from a given Corpus
  m = createDocumentTermMatrix(corpus, maxSparseness)
  rownames(m) = rep(names(corpus)[1], dim(m)[1])
  return(m)
}

createDtms <- function (corpora, maxAllowedSparseness) {
  # Create a list of DocumentTermMatrices from a list of corpora
  data <- lapply(corpora, function (corpus) {
    return (createDtm(corpus, maxAllowedSparseness))
  })
  names(data) = names(corpora)

  return(data)
}

createMainDtm <- function(corpora, sparsenessThreshold) {
  # Create a DocumentTermMatrix from a list of corpora
  dtms = createDtms(corpora, sparsenessThreshold)
  # merge dtms
  dtm = do.call(c, dtms)
  return (dtm)
}

computePerformanceMeasuresForAlgorithm <- function (corpora,
                                                    algorithm,
                                                    sparsenessThreshold,
                                                    attributeSelectionFn,
                                                    k) {
  # attribute selection
  dtm <- createMainDtm(corpora, sparsenessThreshold)
  df <- dataFrameFromDocumentTermMatrix(dtm)
  attributes <- attributeSelectionFn(dtm)

  # select only columns which got to be attributes
  df <- df[,c("class", attributes)]

  folds <- kFoldIndices(dim(df)[1], k)
  measures <- sapply(folds, function (fold) {
    return (computeClassificationMeasuresOnFold(df, algorithm, fold))
  })
  # add means column to the results
  measures <- data.frame(means=rowMeans(apply(measures, 2, as.numeric)), measures)

  # return only means over folds
  means <- t(measures[1])

  # add attribute count
  attributeString <- paste(attributes, collapse='|')
  measuresWithAttrInfo <- cbind(means,
                                attr_count=as.numeric(length(attributes)),
                                attrs=attributeString
                                )

  # remove rownames
  rownames(measuresWithAttrInfo) <- c()

  return (measuresWithAttrInfo)
}

# Ref: http://rali.iro.umontreal.ca/rali/sites/default/files/publis/SokolovaLapalme-JIPM09.pdf
computeClassificationMeasuresOnFold <- function (df, algorithm, fold) {
  # Compute various classification measures for a given algorithm
  #
  # Args:
  # df: dataframe which holds the data
  # algorithm: lambda with algorithm to run on the data
  # fold: a fold with $train and $test which contain test and train indices
  #       of the df data frame respectively
  #
  # Returns:
  #   Row of classification measures
  train <- df[fold$train,]
  test <- df[fold$test,]
  model <- algorithm(train)

  predictions <- predict(model, test[, -1])
  allClasses <- levels(test$class)
  tp <- tn <- fn <- fp <- c()
  allTp <- allTn <- allFn <- allFp <- c()
  accuracy <- errorRate <- precision <- recall <- c()
  for (class in allClasses) {
    # select predictions corresponding to test rows with class 'class'
    classPredictions <- predictions[test[, 1] == class]
    # predictions corresponding to test rows with other classes
    otherPredictions <- predictions[test[, 1] != class]
    allTp[[class]] <- tp <- sum(classPredictions == class)
    allTn[[class]] <- tn <- sum(otherPredictions != class)
    allFn[[class]] <- fn <- sum(classPredictions != class)
    allFp[[class]] <- fp <- sum(otherPredictions == class)


    accuracy[[class]] <- (tp + tn) / (tp + fn + fp + tn)
    errorRate[[class]] <- (fp + fn) / (tp + fn + fp + tn)

    precision[[class]] <- tp / ( tp + fp )
    recall[[class]] <- tp / (tp + fn)

  }
  l <- length(allClasses)
  measures <- c()

  # The average per-class effectiveness of a classiﬁer
  measures$avgAccuracy <- sum(accuracy) / l
  # The average per-class classiﬁcation error
  measures$avgError <- sum(errorRate) / l

  # Agreement of the data class labels with those of a classiﬁers if calculated from sums of per-text decisions
  # measures$avgPrecision <- sum(allTp) / sum(allTp) + sum(allFp) # micro
  # Effectiveness of a classiﬁer to identify class labels if calculated from sums of per-text decisions
  # measures$avgRecall <- sum(allTp) / sum(allTp) + sum(allFp) # micro

  # An average per-class agreement of the data class labels with those of a classiﬁers
  avgPrecision <- measures$avgPrecision <- sum(precision) / l

  # An average per-class effectiveness of a classiﬁer to identify class labels
  avgRecall <- measures$avgRecall <- sum(recall) / l

  measures$avgFscore <- 2 * avgPrecision * avgRecall / ( avgPrecision + avgRecall )
  measures$oldError <- err(test$class, predict(model, test[,-1]))

  return(measures)
}

dataFrameFromDocumentTermMatrix <- function (dtm) {
  # Converts a DocumentTermMatrix to a DataFrame
  class <- rownames(dtm)
  rownames(dtm) <- c()
  df <- as.data.frame(as.matrix(dtm))
  df <- cbind.data.frame(class, df)
  return(df)
}

selectAttributesChiSquared <- function (dtm, cutoff) {
  df <- dataFrameFromDocumentTermMatrix(dtm)

  # Select most relevant attributes from DataFrame given a cutoff
  lst <- chi.squared(df)
  # sort by attr_importance, keep rownames
  sorted <- lst[order(lst[,"attr_importance"]), , drop=FALSE]
  # keep only those bigger than cutoff
  attrs <- sorted[sorted$attr_importance > cutoff, , drop=FALSE]
  return(rownames(attrs))
}

selectAttributesByFrequency <- function (dtm, lowerFreqBound) {
  return (findFreqTerms(dtm, lowfreq = lowerFreqBound))
}


testChiSquaredAttributeSelection <- function (corpora, algorithm) {
  chiSquaredCutOffs <- c(0)
  sparsenessThreshold <- 0.8

  measureMeans <- lapply(chiSquaredCutOffs, function (cutoff) {
    print(paste("cutoff = ", cutoff))
    attributeSelectionFn <- function (df) {
      return (selectAttributesChiSquared(df, cutoff))
    }

    results <- computePerformanceMeasuresForAlgorithm(corpora,
                                                      algorithm,
                                                      0.8,
                                                      attributeSelectionFn,
                                                      3)
    data <- cbind.data.frame(chiSquaredCutOff=cutoff, results)
    rownames(data) <- c()
    return(data)
  })

  results <- do.call(rbind, measureMeans)
  return (results)
}

testFrequencyAttributeSelection <- function (corpora, algorithm) {
  freqs <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  sparsenessThreshold <- 0.8

  measureMeans <- lapply(freqs, function (freqBound) {
    print(paste("freqBound = ", freqBound))
    attributeSelectionFn <- function (dtm) {
      return (selectAttributesByFrequency(dtm, freqBound))
    }

    results <- computePerformanceMeasuresForAlgorithm(corpora,
                                                      algorithm,
                                                      0.8,
                                                      attributeSelectionFn,
                                                      3)
    data <- cbind.data.frame(frequencyLBound=freqBound, results)
    rownames(data) <- c()
    return(data)
  })

  results <- do.call(rbind, measureMeans)
  return (results)
}


Alldirs <- c('alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware', 'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles', 'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space', 'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc', 'talk.religion.misc')
dirs <- sample(Alldirs, 2)
dirs <- Alldirs
# dirs <- c('misc.forsale', 'rec.autos', 'rec.motorcycles', 'rec.sport.baseball', 'rec.sport.hockey')

corpora <- prepCorpora(dirs)

## naiveBayesAlgorithm <- function (trainingSet) {
##   return (naiveBayes(class ~ ., data=trainingSet, laplace = laplace))
## }
## resultsNB <- testFrequencyAttributeSelection(corpora, naiveBayesAlgorithm)

## SVMAlgorithm <- function (trainingSet) {
##   return (svm(class ~ ., data=trainingSet, laplace = laplace))
## }
## resultsSVM <- testFrequencyAttributeSelection(corpora, SVMAlgorithm)

naiveBayesAlgorithm <- function (trainingSet) {
  return (naiveBayes(class ~ ., data=trainingSet, laplace = laplace))
}
resultsNB <- testChiSquaredAttributeSelection(corpora, naiveBayesAlgorithm)

SVMAlgorithm <- function (trainingSet) {
  return (svm(class ~ ., data=trainingSet, laplace = laplace))
}
resultsSVM <- testChiSquaredAttributeSelection(corpora, SVMAlgorithm)
