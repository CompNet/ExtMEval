options(width=Sys.getenv("COLUMNS"))


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
    #indx = which(df1$t == transf.type)
    #df1 = df1[indx,]
    thedata = rbind(thedata, data.frame(score=df1[,"score"],k=df1[,"k"],p=df1[,"p"],h=df1[,"h"],n=df1[,"n"], t=df1[,"t"], m=measures[j]))
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
thedata[,"t"] = as.factor(thedata[,"t"])

# In this formula, we do not include the interaction terms
my.formula = as.formula("score ~ n + k + h + p + m + t + 
				m*t + 
				m*n + m*k + m*h + m*p + 
				t*n + t*k + t*h + t*p + 
				m*t*n + m*t*k + m*t*h + m*t*p")


# In this formula, we do include the interaction terms
# my.formula = as.formula("score ~ n + k + h + p + m + t + 
#				m*t + 
#				m*n + m*k + m*h + m*p +
#				m*n*k + m*n*h + m*n*p + m*k*h + m*k*p + m*h*p +
#				t*n + t*k + t*h + t*p + 
#				t*n*k + t*n*h + t*n*p + t*k*h + t*k*p + t*h*p + 
#				m*t*n + m*t*k + m*t*h + m*t*p + 
#				m*t*n*k + m*t*n*h + m*t*n*p + m*t*k*h + m*t*k*p + m*t*h*p") 

print(my.formula)

# --------------------------------------------------------------------------
# in order to change the reference variables for categorical variables
#thedata <- within(thedata, t <- relevel(t, ref = 3)) # ref.no respects the order in levels(df$t) >> here: Neighbor Cluster Swap

#thedata <- within(thedata, m <- relevel(m, ref = 1)) # ref.no respects the order in levels(df$m) >> here: F-measure
# 1 is Fmeasure
# 2 is FMI
# 3 is HA's ARI
# 4 is JI
# 5 is NMI
# 6 is RI
# --------------------------------------------------------------------------


reg = lm(my.formula, thedata)
print(summary(reg))
print(anova(reg))

