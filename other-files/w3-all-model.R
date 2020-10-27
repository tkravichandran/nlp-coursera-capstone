### Grouping text done once

file1 <- "./final/en_US/en_US.blogs.txt"
file2 <- "./final/en_US/en_US.news.txt"
file3 <- "./final/en_US/en_US.twitter.txt"

res1 <- readLines(file1)
res2<- readLines(file2)
res3 <- readLines(file3)

txt <- c(res1,res2,res3)
n_total <- length(txt);n_total

fileConn<-file("./txt.txt")
writeLines(txt, fileConn)
close(fileConn)

res <- readLines("./txt.txt"); length(res)## tst

system("split -l 50000 -d txt.txt")

########### Defining functions #############

clean <- function(fnam){
    print(getwd())
    txt <- readLines(fnam)

    ## Clean data
    ## Remove some special characters 
    txt <- gsub("['’‘´´]","",txt)
    txt <- gsub(" -","",txt)
    txt <- gsub("p.m","pm",txt)
    txt <- gsub("a.m","am",txt)
    txt <- gsub("A&M","AandM",txt)
    txt <- gsub("[Uu][.][Ss][. ]","US", txt)

    ## Remove other special chatacters
    txt <- gsub("[^A-Za-z0-9 ]"," ",txt)

    corp <- corpus(txt) 
    summary(corp)

    texts(corp) <- gsub("\\s+"," ",texts(corp))

    ## making tokens to cleanup further
    doc.tokens <- tokens(corp, remove_punct=T, remove_numbers=T)
    ## doc.tokens <- tokens_select(doc.tokens, stopwords('english'),selection='remove')
    doc.tokens <- tokens_tolower(doc.tokens)
    doc.tokens <- tokens_select(doc.tokens,min_nchar=2L)
    doc.tokens
}


## Storing Ngrams
l <- list(); l[[1]] <- c("n1");
l[[2]] <- c("n1","n2");
l[[3]] <- c("n1","n2","n3");
l[[4]] <- c("n1","n2","n3","n4");

df_dfm <- function(dfm,ngram){
    df <- data.frame(topfeatures(dfm,n=dim(dfm)[2]));colnames(df)="freq"
    temp <- str_split_fixed(rownames(df), "_", ngram);colnames(temp)=l[[ngram]]
    cdf <- cbind(temp,df);rownames(cdf) <- NULL
    as.data.table(cdf)
}

####### Reading text to datatables and saving CDF's

for (i in 1){
    library(quanteda)
    library(stringr)
    library(data.table)
    set.seed(12345)
    start_time <- Sys.time()
    ## X01-x42 setup
    cnum <- sprintf("%02d",i)
    print(cnum)
    cnam <- paste("x",cnum,sep="")
    ## Doc tokens
    doc.tokens <- clean(cnam)
    print("doc.Tokens cleaning done")
    ## ngrams and dfms
    oneGram <- tokens_ngrams(doc.tokens,n=1)
    dfm1 <- dfm(oneGram)
    ## twoGram <- tokens_ngrams(doc.tokens,n=2)
    ## dfm2 <- dfm(twoGram)
    ## threeGram <- tokens_ngrams(doc.tokens, n=3)
    ## dfm3 <- dfm(threeGram)
    ## fourGram <- tokens_ngrams(doc.tokens, n=4)
    ## dfm4 <- dfm(fourGram)
    print("Grams done")
    ## CDF
    ## cdf4 <- df_dfm(dfm4,4)
    ## cdf3 <- df_dfm(dfm3,3)
    ## cdf2 <- df_dfm(dfm2,2)
    cdf1 <- df_dfm(dfm1,1)
    print("cdf done")
    ## Saving
    ## saveRDS(cdf4,file=paste("cdf4_",cnam,".Rda",sep=""))
    ## saveRDS(cdf3,file=paste("cdf3_",cnam,".Rda",sep=""))
    ## saveRDS(cdf2,file=paste("cdf2_",cnam,".Rda",sep=""))
    saveRDS(cdf1,file=paste("cdf1_",cnam,".Rda",sep=""))
    print("save done")
    time <- Sys.time()-start_time
    print(time)
}


## Remove everything

## gc()
## rm(list=setdiff(ls(),c("i","clean","df_dfm","l"));gc()
### C-c C-e C-r

## Stitching all the CDF's x0-85 to gether
library(quanteda)
library(stringr)
library(data.table)
ngram=1
l <- list(); l[[1]] <- c("n1");
l[[2]] <- c("n1","n2");
l[[3]] <- c("n1","n2","n3");
l[[4]] <- c("n1","n2","n3","n4");

for (i in 2:85){
    start_time <- Sys.time()
    ## X01-xi setup
    print(sprintf("%02d",i))
    fileB <- paste("cdf",ngram,"_x",sprintf("%02d",i),".Rda",sep="")
    if (i != 2){
        fileA <- paste("cdf",ngram,"_main",".Rda",sep="")
    }
    else if (i==2){
        fileA <- paste("cdf",ngram,"_x01",".Rda",sep="")
    }

    DT <- readRDS(fileA)
    dt2 <- readRDS(fileB)
    print(paste("read",fileA,"and",fileB))
    
    DT <- merge(DT,dt2,by=l[[ngram]],all=T)
    rm(dt2);gc()
    DT[is.na(DT)]=0
    DT$freq <- DT$freq.x+DT$freq.y

    DT[,c("freq.x","freq.y") := NULL]
    print("processed DT")
    
    ## then Save Main file
    fileA <- paste("cdf",ngram,"_main",".Rda",sep="")
    saveRDS(DT,fileA)
    time <- Sys.time()-start_time
    print(time)
    print("object size and dimension are ")
    print(object.size(DT)/10^9)
    print(dim(DT))
    rm(DT,dt2);gc()
}





## Merging
## library(data.table)
## dt1 <- readRDS("cdf3_x01.Rda")[6:12,]
## dt2 <- readRDS("cdf3_x02.Rda")[6:15,]

## DT <- merge(dt1,dt2,by=c("n1","n2","n3"),all.x=T)

## DT[is.na(DT)]=0
## DT$freq <- DT$freq.x+DT$freq.y

## DT[,c("freq.x","freq.y") := NULL]

## DT
## dt1

library(data.table)
## cdf4 <- readRDS("cdf4_main.Rda")
## cdf3 <- readRDS("cdf3_main.Rda")
## cdf2 <- readRDS("cdf2_main.Rda")
cdf1 <- readRDS("cdf1_main.Rda")

## order and remove ones!
## cdf4 <- cdf4[order(-freq),]
## cdf3 <- cdf3[order(-freq),]
## cdf2 <- cdf2[order(-freq),]
cdf1 <- cdf[order(-freq),]
