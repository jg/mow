library(tm)
library(tm.plugin.mail)
library(Snowball)
library(SnowballC)

dirs = c('alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware', 'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles', 'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space', 'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc', 'talk.religion.misc')

transformations <- c(as.PlainTextDocument)

prepData <- function(dirname) {
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
alt.atheism <- prepData('alt.atheism')
