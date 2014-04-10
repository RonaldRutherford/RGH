##	SQLite(fetch.default.rec = 500, force.reload = FALSE,shared.cache=FALSE)
##	con <- dbConnect(dbDriver("SQLite"), dbname = "/media/ron80/DataBase/db")
##	dbListTables(con)
##	dbWriteTable(con, "BetaData", summaryData, append = TRUE,row.names=F)
##	names(summaryData) <- c("date1","tic","r_squaredL","betaL","Std_ErrorL")
##	dataB <- dbGetQuery(con, "SELECT * FROM BetaData where date = '2013-04-18'")

library(Unicode)
library(RSQLite)
## library(RMySQL)
SQLite(fetch.default.rec = 500, force.reload = FALSE,shared.cache=FALSE)
con <- dbConnect(dbDriver("SQLite"), dbname = "~/Desktop/DS-Likes/likesT")
dbListTables(con)
## dim(likes)  = 685721   157
##dbGetQuery(con, "create table likes (UID TEXT, CleanLikes TEXT, Likes TEXT)")
##dbGetQuery(con, "create table LikesFreq (CleanLikes TEXT, Counts NUMERIC)")


Rgsub   <- function(namegsub)
{
  gsubnames <- gsub("[!-#&*:\'\ \\)\\(]","", namegsub)
  lenGS <- length(gsubnames)
  try(gsubnames[2:lenGS] <- u_to_lower_case(gsubnames[2:lenGS]),silent = FALSE) 
 return(gsubnames)
 
}

RgsubA   <- function(namegsub)
{
 return(gsubnames <- gsub("[!\'\\)\\(]","", namegsub))
}

## nchar(likesT[1])
## [1] 32

testUID <- function(UIDt)
{
  if(nchar(UIDt) < 31 || nchar(UIDt) > 33) return(FALSE)
  if (nchar(gsub("[^0-9]","", UIDt)) < 10) return(FALSE)
  return (TRUE)
}

insertValues <- function(likesT)
{
  likesC <- Rgsub(likesT)
  wNullC <- which(likesC == "")
  if (length(wNullC)>0) likesC <- likesC[-wNullC]
  cat(likesT)
  cat("\t",UIDi)
  lenL <- length(likesC)
  for (ii in 2:lenL)
  {
    if (!testUID(likesT[ii]))
    {
      if (nchar(likesC[ii]) > 35) next
      dbGetQuery(con, paste("insert into likes values(",paste("'",likesC[1],"','",likesC[ii],"','",RgsubA(likesT[ii]),"'",sep=""),")",sep=""))
      testS <- dbGetQuery(con,paste("select * from LikesFreq where CleanLikes ='",likesC[ii],"'",sep=""))
      if (nrow(testS)==0 ) 
      {
	dbGetQuery(con, paste("insert into LikesFreq values('",likesC[ii],"',1)",sep=""))
      } else  dbGetQuery(con, paste("update LikesFreq set Counts =", testS[2]+1 ," where CleanLikes ='",likesC[ii],"'",sep=""))
    } else  trySplit(likesT[ii:length(likesT)])
  }
  
}

trySplit <- function(likesT)
{
    likesSec <- strsplit(likesT,"\n")[[1]]
    for (ii2 in 1:length(likesSec[1]))
    {
      likeThi <- strsplit(likesSec[1],",")[[1]]
      if (!testUID(likeThi[ii2])) next
      insertValues(likeThi[ii2])
    }
}

## setwd("~/De685721sktop/DataBase/WorkingDirectory")
setwd("~/Desktop/DS-Likes")
## grep -n "6c59be1d0db1a10b9add" likes.csv
## gUID <- "6c59be1d0db1a10b9add746e721e5cd0"


for (UIDi in (135954 - 1):40000)

{
  likesT <- scan("~/Desktop/DS-Likes/likes.csv", what= character(), skip = UIDi, nlines=1,sep=c(",","\n"))
  if (length(likesT)==0) next
  wNullT <- which(likesT == "")
  if (length(wNullT)>0) likesT <- likesT[-wNullT]
  if (!testUID(likesT[1]))
  {
    trySplit(likesT)
  
  } else
  {
    insertValues(likesT)
  }
  
}






