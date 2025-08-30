getTWSE.fiveSecond<-function(ymd=NULL,skip=2,index.names=NULL){

  if(.Platform$OS.type == "unix") {
    Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
  } else {
    Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  }

  if(is.null(ymd)) {ymd=Sys.Date()}
  if (lubridate::wday(ymd,label=TRUE)=="Sun") {ymd=as.Date(ymd)+1} else if (lubridate::wday(ymd)=="Sat") {ymd=as.Date(ymd)-1}
  ymd0=gsub(ymd,pattern = "-",replacement = "")

  twse_5sec=paste0("https://www.twse.com.tw/exchangeReport/MI_5MINS_INDEX?response=csv&date=",ymd0)
  tmp=read.csv(twse_5sec,skip=skip,sep=",",header=FALSE)
  head(tmp,15)

  dat0=tmp[1:3241,-ncol(tmp)]
  HM=paste(ymd, sub("=","",dat0[,1]))
  dat=dat0[,-1]
  dat=sapply(dat0[,-1],function(x) as.numeric(gsub(x, pattern=",", replacement="")))
  rownames(dat)=HM

  if(is.null(index.names)) {colnames(dat)=paste0("V",seq(ncol(dat)))} else {colnames(dat)=index.names}

  return(list(data=dat))

}
