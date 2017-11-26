iClick.lm <- function(dep,indep,data,Formula=NULL,bootrep=99) {

  dat=data

Numeric=NULL;for (j in 1:ncol(dat)) {
Numeric=rbind(Numeric,is.numeric(dat[,j]))}
stringCols=which(Numeric==FALSE)
if(length(stringCols)==0){
summaryTable=as.matrix(fBasics::basicStats(dat)[-c(10:12),])
rownames(summaryTable)=rownames(fBasics::basicStats(dat))[-c(10:12)]
}
if(length(stringCols)>0){
summaryTable1=as.matrix(fBasics::basicStats(dat[,-stringCols])[-c(10:12),])
rownames(summaryTable1)=rownames(fBasics::basicStats(dat[,-stringCols]))[-c(10:12)]
summaryTable2=summary(dat[,stringCols])
summaryTable=list(summaryTable1,summaryTable2)
}
if(!is.null(Formula)) {myformula<-as.formula(Formula)}
if(is.null(Formula)) {
NAMES=colnames(dat)[c(dep,indep)]
myformula<-as.formula(paste(NAMES[1],paste(NAMES[-1],collapse="+"),sep="~"))
}

OUT<-lm(formula=myformula,data=dat)
COEF.HC <- round(unclass(lmtest::coeftest(OUT, vcov = vcovHC(OUT, type = "HC0"))),4)
results=summary(OUT)
COEF.pretty<-papeR::prettify(as.data.frame(round(coef(summary(OUT)),4)))
COEF.HC.pretty= papeR::prettify(as.data.frame(COEF.HC))
HET.test1=lmtest::gqtest(OUT)
HET.test2=lmtest::bptest(OUT)
VIF.test=VIF_no(OUT)
funcForm.test1=lmtest::resettest(OUT)
funcForm.test2=lmtest::raintest(OUT)
funcForm.test3=lmtest::harvtest(OUT)

dataRefreshCode <- function(...)  {
    type = as.integer(.oneClicklm(obj.name = "regType"))
    Unit = colnames(dat)

        # Print Basic Return Statistics:
        if (type == 1) {

        show(summaryTable)
        NAMES=colnames(dat)[c(dep,indep)]
OBJ=as.formula(paste("",paste(NAMES,collapse="+"),sep="~"))
car::scatterplotMatrix(OBJ, reg.line=lm, smooth=TRUE,spread=TRUE, span=0.5, diagonal = "histogram", data=dat)
       }

        # Output Summary
        if (type == 2) {
       cat("\n", "=== Estimation Results Summary ===","\n")
        show(results)
       }

       # Coefficient table
        if (type == 3) {
       cat("\n","1. === Standard covariance ===","\n")
       show(COEF.pretty)

       cat("\n","2. === White robust covariance ===","\n")
       show(COEF.HC.pretty)

       cat("\n","3.=== Confidence Intervals,95% ===","\n")
       show(round(confint(OUT),4))
       }

       # Coefficient plot
        if (type == 4) {
       show(coefplot::coefplot(OUT))
       }

        # Residual diagnosis
        if (type == 5) {
       par(mfrow=c(2,2))
       plot(OUT)
       par(mfrow=c(1,1))
       }

        # Tests for HET
        if (type == 6) {
       cat("===== Tests For multicollinearity =====","\n")
       show(VIF.test)
       }

        # Tests for HET
        if (type == 7) {
       cat("===== Tests For heteroskedasticity =====","\n")
       cat("\n","1.","\n")
       show(HET.test1)
       cat("\n","2.","\n")
       show(HET.test2)
       }

       # Tests for functional forms
        if (type == 8) {
       show(funcForm.test1)
       show(funcForm.test2)
       show(funcForm.test3)
       }

        # bootstrapping
        if (type == 9) {
refit_coef <- function(dat, i) {
boot.tmp<-lm(myformula,data=dat[i,])
coef(boot.tmp)
}
refit_std <- function(dat, i) {
boot.tmp<-lm(myformula,data=dat[i,])
coef(summary(boot.tmp))[,2]
}

boot.coef <- boot::boot(dat,refit_coef, R=bootrep)
boot.std <- boot::boot(dat,refit_std, R=bootrep)

cat("\n","Bootstrap coefficients","\n")
show(summary(boot.coef))
cat("\n","Bootstrap standard errors","\n")
show(summary(boot.std))

cat("\n", "=== White Het corrected estimates ===","\n")
show(round(COEF.HC,4))
       }

       # Save
        if (type == 10) {
Results<-list(OUT.FE=OUT, COEF=coef(summary(OUT)),COEF.HC=COEF.HC, HET.test=list(HET.test1,HET.test2),funcForm.tests=list(funcForm.test1,funcForm.test2,funcForm.test3)

)
  save(Results, file=".lmOUT.RData")
  cat("\n","Outputs saved as ", ".lmOUT.RData","\n")
       }



   cat("=======================================", "\n")

}  #End of dataRefreshCode()

    nAssets = dim(dat)[2]

    .oneClicklm(
        dataRefreshCode,
        names       = c("Selected Asset"),
        minima      = c(      0),
        maxima      = c(      nAssets),
        resolutions = c(      1),
        starts      = c(      0),

        button.functions = list(
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "1")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "2")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "3")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "4")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "5")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "6")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "7")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "8")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "9")
                dataRefreshCode()},
        function(...){
                .oneClicklm(obj.name = "regType", obj.value = "10")
                dataRefreshCode()}
        ),

        button.names = c(
            " 1 Scatterplot Matrix and Descriptive Statistics",
            " 2 Estimation Results Summary",
            " 3 More Coefficients Outputs",
            " 4 Coefficients plot",
            " 5 Residual Diagnosis",
            " 6 VIF test for multicolliniarity",
            " 7 Tests for heteroskedasticity",
            " 8 Tests for functional form",
paste(" 9 Bootstrapping, selected replications= ", bootrep,sep=""),
            "10 Save output: .lmOUT.RData"),

        title = "1-Click Linear Model"
        )

  .oneClicklm(obj.name = "type", obj.value = "1", no = 1)

   # Return Value()
   invisible()

}


.oneClicklm.env = new.env()


.oneClicklm <-
  function(names, minima, maxima, resolutions, starts,button.functions, button.names, no, set.no.value, obj.name, obj.value,reset.function, title)
  {

     if(!exists(".oneClicklm.env")) {
      .oneClicklm.env <<- new.env()
    }
    if(!missing(obj.name)){
      if(!missing(obj.value)) {
        assign(obj.name, obj.value, envir = .oneClicklm.env)
      } else {
        obj.value <- get(obj.name, envir = .oneClicklm.env)
      }
      return(obj.value)
    }
    if(missing(title)) {
      title = "Control Widget"
    }

    # GUI Settings:
    myPane <- tktoplevel()
    tkwm.title(myPane, title)
    tkwm.geometry(myPane, "+0+0")

    # Buttons:
    framed.button <- ttkframe(myPane,padding=c(3,3,12,12))
    tkpack(framed.button, fill = "x")

    if (missing(button.names)) {
      button.names <- NULL
    }

#loop through button names
    for (i in seq(button.names)) {
      button.fun <-button.functions[[i]]
      plotButtons<-tkbutton(framed.button, text = button.names[i], command = button.fun, anchor = "nw",relief="ridge",width = "45")
      tkconfigure(plotButtons,foreground="blue",font=tkfont.create(size=10,weight="bold"))
      tkpack(plotButtons,fill = "x", pady=1)

}


  #===== Quit Button:
    quitCMD = function() {tkdestroy(myPane)}

   quitButton<-tkbutton(framed.button, text = "Quit", command = quitCMD, anchor = "center",relief="ridge",width = "8")
   tkbind(myPane,"Q",function() tcl(quitButton,"invoke"))
   tkfocus(quitButton)
   tkconfigure(quitButton,foreground="indianred2", font=tkfont.create(weight="bold",size=10))

   tkconfigure(quitButton,underline=0)
   tkpack(quitButton, side = "right",fill = "x",ipady=3)


    assign(".oneClicklm.values.old", starts, envir = .oneClicklm.env)

    # Return Value:
    invisible(myPane)
  }



