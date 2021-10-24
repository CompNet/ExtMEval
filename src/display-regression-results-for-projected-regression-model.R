options(width=Sys.getenv("COLUMNS"))

#transf.type = TRANSF.ORT.CLU
#transf.type = TRANSF.1NEWCLU
#transf.type = TRANSF.NEIG.CLU.SWAP
transf.type = TRANSF.kNEWCLU
#transf.type = TRANSF.SINGLCLU



print(transf.type)
print("------")
data.frame.folder = DATA.FRAME.FOLDER

measures = COMP.MEASURES

simpl.measures =  gsub("[^[:alnum:]]", " ", measures) # remove special characters
simpl.measures = tolower(gsub(" ", "", simpl.measures)) # remove whitespaces

ord = order(simpl.measures) # order measures, since factor() will order them alphabetically
simpl.measures = simpl.measures[ord]
measures = measures[ord]

thedata = c()
for(j in 1:length(measures)){
    fpath = file.path(data.frame.folder, paste0(measures[j],"-data-frame.csv"))
    print(fpath)
    df1 <- read.csv(fpath, sep=";", stringsAsFactors=F, check.names=F, header=T)
    indx = which(df1$t == transf.type)
    df1 = df1[indx,]
    thedata = rbind(thedata, data.frame(score=df1[,"score"],k=df1[,"k"],p=df1[,"p"],h=df1[,"h"],n=df1[,"n"], m=measures[j]))
}

#thedata[,"n"] =thedata[,"n"]-mean(thedata[,"n"])
thedata[,"n"] = scale(thedata[,"n"])
#thedata[,"k"] = thedata[,"k"]-mean(thedata[,"k"])
thedata[,"k"] = scale(thedata[,"k"])
#thedata[,"p"] = thedata[,"p"]-mean(thedata[,"p"])
thedata[,"p"] = scale(thedata[,"p"])
#thedata[,"h"] = thedata[,"h"]-mean(thedata[,"h"])
thedata[,"h"] = scale(thedata[,"h"])
thedata[,"m"] = as.factor(thedata[,"m"])

# --------------------------------------------------------------------------
# in order to change the reference variables for categorical variables

thedata <- within(thedata, m <- relevel(m, ref = 1)) # fix RI as the ref group >> 5 is the NMI
# 1 is Fmeasure
# 2 is FMI
# 3 is HA's ARI
# 4 is JI
# 5 is NMI
# 6 is RI
# --------------------------------------------------------------------------

my.formula = as.formula("score ~ n + k + h + p + m + m*n + m*k + m*h + m*p")
print(my.formula)
reg = lm(my.formula, thedata)
print(summary(reg))
print(anova(reg))

