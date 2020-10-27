t1 <- Sys.time()
string.q3 <-
    c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
      "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
      "I'd give anything to see arctic monkeys this",
      "Talking to your mom has the same effect as a hug and helps reduce your",
      "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
      "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
      "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
      "Every inch of you is perfect from the bottom to the",
      "I’m thankful my childhood was filled with imagination and bruises from playing",
      "I like how the same people are in almost all of Adam Sandler's")


########################################### Actual algorithm #########

library(tm)
library(quanteda)
library(data.table)
options(stringsAsFactors=FALSE)

setwd("~/agent18/DSS/assignments/assignment-c10-w1/")
cdf4 <- readRDS("cdf4_korter.Rda")
cdf3 <- readRDS("cdf3_korter.Rda")
cdf2 <- readRDS("cdf2_korter.Rda")
cdf1 <- readRDS("cdf1_sc.Rda")

clean <- function(txt,n){
    txt <- gsub("’","",txt)
    txt <- gsub("‘","",txt)
    txt <- gsub("´","",txt)
    txt <- gsub("`","",txt)
    txt <- gsub("'","",txt)
    txt <- gsub(" -","",txt)
    txt <- gsub("p.m","pm",txt)
    txt <- gsub("a.m","am",txt)
    txt <- gsub("A&M","AandM",txt)
    txt <- gsub("[Uu][.][Ss][. ]","US", txt)

### Remove other special chatacters
    txt <- gsub("[0-9?&/\\-]"," ",txt)
    txt <- gsub("\r"," ",txt)
    txt <- gsub("\n"," ",txt)
    txt <- gsub("/"," ",txt)

### remove stray characters
    txt <- gsub(" [A-Za-z] "," ",txt)

    corp <- corpus(txt) 
    texts(corp) <- gsub("\\s+"," ",texts(corp))

### making tokens to cleanup further

    doc.tokens <- tokens(corp, remove_punct=T, remove_numbers=T)
    ##doc.tokens <- tokens_select(doc.tokens, stopwords('english'),selection='remove')
    doc.tokens <- tokens_tolower(doc.tokens)

    doc.tokens <- tokens_select(doc.tokens,min_nchar=2L)
    doc.tokens[[1]]
}


predict.baseline <- function(x){
    tokenss <- clean(x)
    tokens3 <- tail(tokenss,3)
    tokens2 <- tail(tokenss,2)
    tokens1 <- tail(tokenss,1)
    tokens3

    ## Probability model rebuild
    options(stringsAsFactors=FALSE)
    dt <- data.table(pred=as.character(),prob=numeric(),ngram=numeric(),stringsAsFactors=FALSE)
    dt
    ### 4-gram
    act4 <- cdf4[cdf4$n1==tokens3[1] & cdf4$n2==tokens3[2] & cdf4$n3==tokens3[3],]
    sum3 <- cdf3[cdf3$n1==tokens3[1] & cdf3$n2==tokens3[2] & cdf3$n3==tokens3[3],]

    if (nrow(act4)>0 & nrow(sum3)>0){
        print(act4[1:5,])
        print(sum3[1:2])
        dt <- rbind(dt,data.table(pred=act4$n4[1:5],prob=act4$freq[1:5]/sum3$freq,ngram=4))
        print(dt)
    } else {
        print("act4 or sum3 is empty")
    }

### 3-gram
    act3 <- cdf3[cdf3$n1==tokens2[1] & cdf3$n2==tokens2[2],]
    sum2 <- cdf2[cdf2$n1==tokens2[1] & cdf2$n2==tokens2[2],]

    if (nrow(act3)>0 & nrow(sum2)>0){
        print(act3[1:5,])
        print(sum2[1:2])
        dt <- rbind(dt,data.table(pred=act3$n3[1:5],prob=act3$freq[1:5]/sum2$freq,ngram=3))
        print(dt)
    } else {
        print("act3 or sum2 is empty")
    }

### 2-gram
    act2 <- cdf2[cdf2$n1==tokens1[1],]
    sum1 <- cdf1[cdf1$n1==tokens1[1],]

    if (nrow(act2)>0 & nrow(sum1)>0){
        print(act2[1:5,])
        print(sum1[1:2])
        dt <- rbind(dt,data.table(pred=act2$n2[1:5],prob=act2$freq[1:5]/sum1$freq,ngram=2))
        print(dt)
    } else {
        print("act2 or sum1 is empty")
    }

    dt <- dt[complete.cases(dt),]## remove null set pull set
    dt <- dt[order(-prob),]## order eedho anima
    dt <- dt[!duplicated(dt$pred),]

    n=3
    if(dim(dt)[1]<n){

        n_pred <- dim(dt)[1]
        
        ind <-  sample(1:nrow(cdf1),n-n_pred)
        dt <- rbind(dt,data.table(pred=cdf1$n1[ind],prob=0.00001,ngram=1))        
    }
    print(dt)
    dt$pred[1:3]
    
}

x <- string.q3[10]
print(predict.baseline(x))
Sys.time()-t1
