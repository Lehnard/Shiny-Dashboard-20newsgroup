wordFreq <- function(newsGrp_DT) {
        
        # Applies to "Content" column the processText() function element-wise.
        # We asume that newsGrp_DT has been text processed before. 
        # newsGrp_DT[ , Content:=processText(Content)]
        
        # Declara "palCorpus", una estructura Corpus de tm package, con DF.
        # Source: VectorSource. Interprets each element of the vector x as a document.
        corpus <- Corpus(VectorSource(newsGrp_DT[,"Content"]))    
        
        # document term matrix using "tm" package.
        dtm <- DocumentTermMatrix(corpus)
        
        # matrix coertion.
        m <- as.matrix(dtm)
        
        # term vector
        v <- sort(colSums(m), decreasing= TRUE)
        
        # word-frecuency dataframe        
        d <- data.frame(word.number= 1:length(v), word= names(v), frecuency= v)
        row.names(d) <- NULL
        
        return(d)
}