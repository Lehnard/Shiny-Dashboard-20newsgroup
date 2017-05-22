# ===================================================================================
# Script que parte el dataset original de 19997 documentos en subgrupos segun los 
# newsgroups.
# 
# 
# ===================================================================================

library(tm)
library(data.table)

source("../scripts/process_texts.R")

load("../data/DT.RData")

# News Group names without punctuation
newsGrpLabel <- levels(as.factor(DT$Tag))

corpus <- Corpus(VectorSource(newsGrpLabel))
corpus <- tm_map(corpus, removePunctuation)

groupnames <- corpus[1:20]$content

for (i in 1:20) {

        cat(i, "\t", newsGrpLabel[i], "\n")
        DF <- DT[Tag==newsGrpLabel[i]]
        
        class(DF) <- "data.frame"
        
        proc_corpus <- process_texts(DF$Content, stem= TRUE)
        proc_content <- as.character(lapply(proc_corpus, '[[', "content" ))
        
        DF["Content"] <- proc_content
        
        assign(newsGrpLabel[i], DF)
        
        fileName <- c("../newsData/",groupnames[i],".RData")
        fileName <- paste(fileName, collapse = "")

        save(list= newsGrpLabel[i], file= fileName)
}

rm(DF, fileName,i, corpus, DT)

# save(newsGrpLabel, file = "newsGrpNames.RData")

