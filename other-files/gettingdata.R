
url <- "./final/en_US/en_US.twitter.txt"
res <- readLines(url)

length(readLines(file))

sres <- nchar(res)

max(sres)
which.max(sres)


url <- "./final/en_US/en_US.twitter.txt"
res <- readLines(url)

gr <- grepl(".love",res)

head(res[gr])

br <- grepl(".hate",res)

head(res[br])

length(res[gr])/length(res[br])


ot <- grepl(".biostats",res)

head(res[ot])

tt <-
    grepl(". computer once beat me at chess, but it was no match for me at kickboxing",
          res)

head(res[t])
