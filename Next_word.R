library(readtext)
library(quanteda)
library(data.table)
library(stringi)
library(stringr)

UniFreq<-readRDS('UniFreq.rds')
BiFreq<-readRDS('BiFreq.rds')
TriFreq<-readRDS('TriFreq.rds')

get.obs.NGrams.by.pre <- function(wordseq, NgramFreq) {
  PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
  NgramFreq[grep(PreTxt, NgramFreq[,term], perl=T, useBytes=T),]
}

get.unobs.Ngram.tails <- function(ObsNgrams, N) {
  ObsTails <- str_split_fixed(ObsNgrams[,term], "_", N)[,N]
  return(data.table(term=UniFreq[!ObsTails,term,on="term"]))
}

cal.obs.prob <- function(ObsNgrams, Nm1Grams, wordseq) {
  PreCount <- Nm1Grams[wordseq, c, on=.(term)]
  ObsNgrams[,Prob:=ObsNgrams[,cDis]/PreCount]  # c_dis/c
}

cal.alpha <- function(ObsNGrams, Nm1Grams, wordseq) {
  if (dim(ObsNGrams)[1] != 0) {
    # return(1-sum(ObsNGrams[,.(Qbo)]))  # We don't use this formular because End Of Sentence is not counted
    return(sum(ObsNGrams[,c-cDis]/Nm1Grams[wordseq, c, on=.(term)]))
  } else {
    return(1)
  }
}

Find_Next_word <- function(xy, words_num) {
  xy <- gsub(" ", "_", xy)
  if (length(which(BiFreq$term == xy)) > 0) {  # C(x,y) > 0
    ## N-grams preparation
    # Retrieve all observed trigrams beginning with xy: OT
    ObsTriG <- get.obs.NGrams.by.pre(xy, TriFreq)
    y <- str_split_fixed(xy,"_", 2)[,2]
    # Retrieve all observed bigrams beginning with y: OB
    ObsBiG <- get.obs.NGrams.by.pre(y, BiFreq)
    # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0, UOB in UOT
    UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2)
    # Exclude observed bigrams that also appear in observed trigrams: OB in UOT
    ObsBiG <- ObsBiG[!str_split_fixed(ObsTriG[,term], "_", 2)[,2], on="term"]
    
    ## Calculation part
    # Calculate probabilities of all observed trigrams: P^*(z|x,y)
    ObsTriG <- cal.obs.prob(ObsTriG, BiFreq, xy)
    # Calculate Alpha(x,y)
    Alpha_xy <- cal.alpha(ObsTriG, BiFreq, xy)
    # Calculate probabilities of all observed bigrams: P^*(z|y), (y,z) in UOT
    ObsBiG <- cal.obs.prob(ObsBiG, UniFreq, y)
    # Calculate Alpha(y)
    Alpha_y <- cal.alpha(ObsBiG, UniFreq, y)
    # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
    UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
    UnObsBiTails[, Prob:=Alpha_xy*Alpha_y*Prob]
    UnObsBiTails[, c:=UniFreq[UnObsBiTails, c, on=.(term)]]
    UnObsBiTails[, cDis:=UniFreq[UnObsBiTails, cDis, on=.(term)]]
    # Remove unused column in ObsTriG and ObsBiG
    #ObsTriG[, c("c", "cDis"):=NULL]
    ObsTriG[, term:=str_remove(ObsTriG[, term], "([^_]+_)+")]
    #ObsBiG[, c("c", "cDis"):=NULL]
    ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
    # Compare OT, Alpha_xy * P_{Katz}(z|y)
    # P_{Katz}(z|y) = 1. P^*(z|y), 2. Alpha_y * P_{ML}(z)
    ObsBiG[,Prob:=Alpha_xy*Prob]
    AllTriG <- setorder(rbind(ObsTriG, ObsBiG, UnObsBiTails), -Prob)
    return(AllTriG[Prob!=0][1:min(dim(AllTriG[Prob!=0])[1], words_num)])
  } else {  # C(x,y) = 0
    y <- str_split_fixed(xy,"_", 2)[,2]
    # c(y>0)
    if (length(which(UniFreq$term == y)) > 0) {
      # Retrieve all observed bigrams beginning with y: OB
      ObsBiG <- get.obs.NGrams.by.pre(y, BiFreq)
      # Calculate probabilities of all observed bigrams: P^*(z|y)
      ObsBiG <- cal.obs.prob(ObsBiG, UniFreq, y)
      # Calculate Alpha(y)
      Alpha_y <- cal.alpha(ObsBiG, UniFreq, y)
      # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0
      UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2)
      # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
      UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
      UnObsBiTails[, Prob:=Alpha_xy*Alpha_y*Prob]
      UnObsBiTails[, c:=UniFreq[UnObsBiTails, c, on=.(term)]]
      UnObsBiTails[, cDis:=UniFreq[UnObsBiTails, cDis, on=.(term)]]
      # Remove unused column in ObsBiG
      #ObsBiG[, c("c", "cDis"):=NULL]
      ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
      AllBiG <- setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
      return(AllBiG[Prob!=0][1:words_num])
    } else {  # c(y=0)
      # P^*z
      return(setorder(UniFreq, -cDis)[1:words_num,.(term, c, cDis,Prob=cDis/UniFreq[,sum(c)])])
      #return(setorder(UniFreq, -cDis)[1:words_num,.(term, Prob=cDis/UniFreq[,sum(c)])])  
    }
  }
}


Preprocess <- function(wordseq) {
  names(wordseq) <- NULL
  quest <- wordseq %>% tokens(remove_numbers=T, remove_punct=T, remove_symbols=T, split_hyphens=T,what = "word1",remove_url=T) %>% tokens_remove(stopwords("en")) %>% tokens_tolower()
  return(paste(tail(quest[[1]], 2), collapse = " "))
}


Next_word <- function(prephrase, words_num=100) {
  bigr <- Preprocess(prephrase)
  result <- Find_Next_word(bigr, words_num)
  if (dim(result)[1] == 0) {
    rbind(result, list("<Please input more text>", 1))
  }
  return(result)
}