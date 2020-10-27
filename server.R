library(shiny)
shinyServer(function(input, output,session) {
    
    ## Initializations of function
    library(tm)
    library(quanteda)
    library(data.table)
    options(stringsAsFactors=FALSE)

    ## Initializations of UI
    updateTextInput(session,"i1",value="Please enter the text")
    output$t1 <- renderUI({
        tags$button("No Prediction Yet")
    })
    ## output$t1 <- renderText("1. No prediction yet")
    ## output$t2 <- renderText("2. No prediction yet")
    ## output$t3 <- renderText("3. No prediction yet")
    ## output$myList <-
    ##     renderUI(HTML("<ul><li>...text...</li><li>...more text...</li></ul>"))
    
    cdf4 <<- readRDS("cdf4_korter.Rda")
    cdf3 <<- readRDS("cdf3_korter.Rda")
    cdf2 <<- readRDS("cdf2_korter.Rda")
    cdf1 <<- readRDS("cdf1_sc.Rda")

    ## Prediction button
    observeEvent(input$do, {
        t1 <- Sys.time()
        pred.list <- predict.baseline(input$i1)
        output$t1 <- renderUI(tags$button(paste("1.",pred.list$pred[1])))
        output$t2 <- renderUI(tags$button(paste("2.",pred.list$pred[2])))
        output$t3 <- renderUI(tags$button(paste("3.",pred.list$pred[3])))
        
        t.diff <- sprintf((Sys.time()-t1), fmt = '%#.2f')
        output$t4 <-
            renderText(paste("Time taken for prediction is",t.diff,"s"))

        output$table1 <- renderTable(pred.list[1:5])
        ## output$t1 = renderText({pred.list[1]})
        ## output$t2 = renderText({pred.list[2]})
        ## output$t3 = renderText({pred.list[3]})  
        
        ##output$t1 = renderText(a)
    })

    observeEvent(input$clear, {
        output$t1 <- renderUI("")
        output$t2 <- renderUI("")
        output$t3 <- renderUI("")
        output$t4 <- renderText("")
        output$table1 <- renderTable("")
        updateTextInput(session,"i1",value="Please enter the text")
    })
    
})


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
    if(exists("cdf4")==FALSE){
        cdf4 <- readRDS("cdf4_sc.Rda")
        cdf3 <- readRDS("cdf3_sc_kort.Rda")
        cdf2 <- readRDS("cdf2_sc_kort.Rda")
        cdf1 <- readRDS("cdf1_sc_kort.Rda")
    }
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

    n=5
    if(dim(dt)[1]<n){

        n_pred <- dim(dt)[1]
        
        ind <-  sample(1:nrow(cdf1),n-n_pred)
        dt <- rbind(dt,data.table(pred=cdf1$n1[ind],prob=0.00001,ngram=1))        
    }
    print(dt)
    dt
}
