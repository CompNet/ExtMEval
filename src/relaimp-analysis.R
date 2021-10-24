

###############################################################################################
# It performs regression and relative importance analysis over all measures, considered for each transformation type.
# 
# Relative importance refers to the contribution of an independent variable, by itself and in combination with other independent variables,
#  to the prediction or the explanation of the dependent variable. we select the most straightforward approach, consisting in 
#  using squared standardized regression coefficients (SRC), also called squared beta weights.
#
# Note that in this method we opt for performing this analysis for a fixed evaluation transformation type. 
# And recall that our first goal is to estimate effect sizes of the regression terms (i.e. relative importance analysis).
# At this stage, we do not consider the significance analysis.
# We could also do the same analysis over all transformation types for a fixed measure.
# Moreover, we could do the same analysis over all measures and transformation types without fixing any variable.
# Even we could do the same analysis for a fixed transformation type and a fixed measure.
# All these models give the same results, since our independent variables are perfectly uncorrelated by design.
#
#
# measures: all considered/selected evaluation measures
# transf.type: considered transformation type among the selected ones
# data.frame.folder: the folder path, where the data frame results are found
# raw.results.folder: the folder path, where raw regression results will be stored
#
###############################################################################################
#compare.measures.by.transf = function(measures, transf.type, data.frame.folder, raw.results.folder){
#
#    simpl.measures =  gsub("[^[:alnum:]]", " ", measures) # remove special characters
#    simpl.measures = tolower(gsub(" ", "", simpl.measures)) # remove whitespaces
#
#    ord = order(simpl.measures) # order measures, since factor() will order them alphabetically
#    simpl.measures = simpl.measures[ord]
#    measures = measures[ord]
#
#    df = c()
#    for(i in 1:length(measures)){
#        df1 <- read.csv(file.path(data.frame.folder, paste0(measures[i],"-data-frame.csv")), sep=";", stringsAsFactors=F, check.names=F, header=T)
#        indx = which(df1$t == transf.type)
#        df1 = df1[indx,]
#        df1$m = simpl.measures[i]
#        df = rbind(df, df1)
#    }
#    df$m = as.factor(df$m) 
#
#    library(ade4)
#    Z = acm.disjonctif(as.data.frame(df[,c("m")]))  # this produces the complete disjunctive table of a factor table. In our case, measure is a factor
#    thedata = data.frame(cbind(df[,c("score","n","k","h","p")], Z))
#    colnames(thedata) = c("score","n","k","h","p", simpl.measures)
#
#    #thedata[,"n"] = thedata[,"n"]/max(thedata[,"n"])
#    #thedata[,"k"] = thedata[,"k"]/max(thedata[,"k"])
#
#    thedata[,"n"] = c(scale(thedata[,"n"]))
#    thedata[,"k"] = c(scale(thedata[,"k"]))
#    thedata[,"p"] = c(scale(thedata[,"p"]))
#    thedata[,"h"] = c(scale(thedata[,"h"]))
#
#    singles = c()
#    for(d in simpl.measures)
#      singles = c(singles, d)
#
#    pairs = c()
#    for(d in c("n","k","h","p")){
#        pairs = c(pairs, paste0(singles, ":", d))
#    }
#
#    triples = c()
#    A = combn(c("n","k","h","p"), 2)
#    for(i in 1:ncol(A)){
#        pair = A[,i]
#            triples = c(triples, paste0(singles, ":", pair[1],":",pair[2]))
#    }
#
#    my.formula = as.formula(paste0("score ~ 0 + ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + "), " + ",  paste0(triples, collapse=" + ")))
#    #my.formula = as.formula(paste0("score ~ ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + "), " + ",  paste0(triples, collapse=" + ")))
#    print(my.formula)
#    reg = lm(my.formula, thedata) # the first single term as ref variable, so omitted ==>> coeff of the reg model
#
#    sumreg = summary(reg)
#    sink(file.path(raw.results.folder, paste0("summary-reg_","compare-measures-by-transf=",transf.type,".txt")))
#    print(sumreg)
#    sink()  # returns output to the console
#
#    anoreg = anova(reg)
#    print(anoreg)
#    library(car)
#    print("anova type 2")
#    print(Anova(reg, type=2))
#    print("anova type 3")
#    print(Anova(reg, type=3))
#    print(sum(anoreg$`Sum Sq`))
#
#
#    write.csv(x=anoreg, file=file.path(raw.results.folder, paste0("anova-reg_","compare-measures-by-transf=",transf.type,".csv")))
#
#    coeffs = reg$coefficients
#    sumsq = round(anoreg$`Sum Sq`,3)[-length(anoreg$`Sum Sq`)] # remove info related to residuals    
#
#    labels = rownames(anoreg)[-length(rownames(anoreg))] # remove info related to residuals
#    ss = strsplit(labels, split=":")
#
#    labels.by.measure = list()
#    indx.by.measure = list()
#    coeffs.by.measure = list()
#    sumsq.by.measure = list()
#    for(i in 1:length(ss)){
#        vec = ss[[i]]
#        if(length(vec) == 1){
#            labels.by.measure[[ vec[1] ]] = c("measure")
#            indx.by.measure[[ vec[1] ]] = c(indx.by.measure[[ vec[1] ]], i)
#        }
#        else{
#            labels.by.measure[[ vec[1] ]] = c( labels.by.measure[[ vec[1] ]], paste0(vec[2:length(vec)], collapse=":") )
#            indx.by.measure[[ vec[1] ]] = c(indx.by.measure[[ vec[1] ]], i)
#        }
#    }
#
#    for(measure in simpl.measures){
#        # coeffs
#        vec = coeffs[ indx.by.measure[[measure]] ]
#        names(vec) = labels.by.measure[[measure]]
#        coeffs.by.measure[[measure]] = vec
#
#        # sumsq
#        vec = sumsq[ indx.by.measure[[measure]] ]
#        names(vec) = labels.by.measure[[measure]]
#        sumsq.by.measure[[measure]] = vec
#    }
#
#    coeffs.df = do.call(cbind, coeffs.by.measure)
#    sumsq.df = do.call(cbind, sumsq.by.measure)
#
#
#    # post processing, label measures correctly, as appeared in csv files. In regression, we had to remove whitespace, etc.
#    colnames(coeffs.df) = measures
#    colnames(sumsq.df) = measures
#
#    result = list(coeffs=coeffs.df, sumsq=sumsq.df)
#    return(result)
#}




###############################################################################################
# It performs regression and relative importance analysis over all transformation types, considered for each measure.
# 
# Relative importance refers to the contribution of an independent variable, by itself and in combination with other independent variables,
#  to the prediction or the explanation of the dependent variable. we select the most straightforward approach, consisting in 
#  using squared standardized regression coefficients (SRC), also called squared beta weights.
#
# Note that in this method we opt for performing this analysis for a fixed evaluation measure. 
# And recall that our first goal is to estimate effect sizes of the regression terms (i.e. relative importance analysis).
# At this stage, we do not consider the significance analysis.
# Having this mind, we could also do the same analysis over all measures for a fixed transformation type.
# Moreover, we could do the same analysis over all measures and transformation types without fixing any variable.
# Even we could do the same analysis for a fixed transformation type and a fixed measure.
# All these models give the same results, since our independent variables are perfectly uncorrelated by design.
#
#
# measure: considered evaluation measure among the selected ones
# data.frame.folder: the folder path, where the data frame results are found
# raw.results.folder: the folder path, where raw regression results will be stored
#
###############################################################################################
compare.transfs.by.measure = function(measure, data.frame.folder, raw.results.folder){

    print("compare.transfs.by.measure")
    df<-read.csv(file.path(data.frame.folder, paste0(measure,"-data-frame.csv")), sep=";", stringsAsFactors=F, check.names=F, header=T)
    orig.transf.types = unique(df$t)
    df[,"t"] = gsub(" ", "", df[,"t"]) # remove whitespace
    indx = which(df[,"t"] == "1NewCluster")
    df[indx,"t"] = "OneNewCluster"
    df$t = as.factor(df$t)

    library(ade4)
    Z = acm.disjonctif(as.data.frame(df[,c("t")])) # this produces the complete disjunctive table of a factor table.
													# In our case, transformation type is a factor, i.e. categorical variable
    
	thedata = data.frame(cbind(df[,c("score","n","k","h","p")], Z))
    colnames(thedata) = c("score","n","k","h","p", levels(df$t))

    #thedata[,"n"] = thedata[,"n"]/max(thedata[,"n"])
    #thedata[,"k"] = thedata[,"k"]/max(thedata[,"k"])

    thedata[,"n"] = c(scale(thedata[,"n"]))
    thedata[,"k"] = c(scale(thedata[,"k"]))
    thedata[,"p"] = c(scale(thedata[,"p"]))
    thedata[,"h"] = c(scale(thedata[,"h"]))

    #print(summary(thedata[,"n"]))
    #print(summary(thedata[,"k"]))

    transf.types = levels(df$t)

    singles = c()
    for(d in levels(df$t))
      singles = c(singles, d)

    pairs = c()
    for(d in c("n","k","h","p")){
        pairs = c(pairs, paste0(singles, ":", d))
    }

    triples = c()
    A = combn(c("n","k","h","p"), 2)
    for(i in 1:ncol(A)){
        pair = A[,i]
            triples = c(triples, paste0(singles, ":", pair[1],":",pair[2]))
    }

    my.formula = as.formula(paste0("score ~ 0 + ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + "), " + ",  paste0(triples, collapse=" + ")))
    #my.formula = as.formula(paste0("score ~ ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + "), " + ",  paste0(triples, collapse=" + ")))
    #print(my.formula)
    reg = lm(my.formula, thedata) # the first single term as ref variable, so omitted ==>> coeff of the reg model

    print("done for model reg -------------------")

    sumreg = summary(reg)
    sink(file.path(raw.results.folder, paste0("summary-reg_","compare-transfs-by-measure=",measure,".txt")))
    print(sumreg)
    sink()  # returns output to the console
    
    anoreg = anova(reg)
    write.csv(x=anoreg, file=file.path(raw.results.folder, paste0("anova-reg_","compare-transfs-by-measure=",measure,".csv")))

    coeffs = reg$coefficients
    sumsq = round(anoreg$`Sum Sq`,3)[-length(anoreg$`Sum Sq`)] # remove info related to residuals    

    labels = rownames(anoreg)[-length(rownames(anoreg))] # remove info related to residuals
    ss = strsplit(labels, split=":")

    labels.by.transf = list()
    indx.by.transf = list()
    coeffs.by.transf = list()
    sumsq.by.transf = list()
    for(i in 1:length(ss)){
        vec = ss[[i]]
        if(length(vec) == 1){
            labels.by.transf[[ vec[1] ]] = c("transf")
            indx.by.transf[[ vec[1] ]] = c(indx.by.transf[[ vec[1] ]], i)
        }
        else{
            labels.by.transf[[ vec[1] ]] = c( labels.by.transf[[ vec[1] ]], paste0(vec[2:length(vec)], collapse=":") )
            indx.by.transf[[ vec[1] ]] = c(indx.by.transf[[ vec[1] ]], i)
        }
    }

    for(transf in transf.types){
        # coeffs
        vec = coeffs[ indx.by.transf[[transf]] ]
        names(vec) = labels.by.transf[[transf]]
        coeffs.by.transf[[transf]] = vec

        # sum of squares
        vec = sumsq[ indx.by.transf[[transf]] ]
        names(vec) = labels.by.transf[[transf]]
        sumsq.by.transf[[transf]] = vec
    }

    coeffs.df = do.call(cbind, coeffs.by.transf)
    sumsq.df = do.call(cbind, sumsq.by.transf)


    # post processing, label transf types correctly, as appeared in csv files. In regression, we had to remove whitespace, etc.
    orig.transf.types2 = orig.transf.types
    indx = which(orig.transf.types2 == "1 New Cluster")
    orig.transf.types2[indx] = "One New Cluster"
    ord = order(orig.transf.types2) # ordered, since factor() orders it alphabetically
    ord.orig.transf.types = orig.transf.types[ord]
    colnames(coeffs.df) = ord.orig.transf.types
    colnames(sumsq.df) = ord.orig.transf.types

    result = list(coeffs=coeffs.df, sumsq=sumsq.df)
    return(result)
}




###############################################################################################
# It is the starting method, which performs regression and relative importance analysis. 
# It also stores the results into files.
#
# transf.types: considered transformation types
# comp.measures: considered evaluation measures
#
###############################################################################################
compute.all.relaimp = function(transf.types, comp.measures)
{
    tlog("compute all relaimp")

	data.frame.folder = DATA.FRAME.FOLDER
    tlog(4, "data frame folder is: ", data.frame.folder)
    if(!dir.exists(data.frame.folder))
        return(0)

    raw.results.folder = REGRESSION.RAW.RES.FOLDER
    if(!dir.exists(raw.results.folder))
        dir.create(raw.results.folder, recursive=TRUE)

    comparison.folder = REGRESSION.COMP.RES.FOLDER
    if(!dir.exists(comparison.folder))
        dir.create(comparison.folder, recursive=TRUE)

	
	# ==================================================================================================
	## handling the results by measure: writing into files 
	
    all.coeffs = c()
    all.sumsq = c()
    for(measure in comp.measures){
        result = compare.transfs.by.measure(measure, data.frame.folder, raw.results.folder)

        coeffs = result$coeffs
        write.csv(x=coeffs, file=file.path(comparison.folder, paste0("measure=",measure,"-coeffs.csv")))
        colnames(coeffs) = paste0(measure,",",colnames(coeffs))
        all.coeffs = cbind(all.coeffs, coeffs)


        sumsq = result$sumsq
        write.csv(x=sumsq, file=file.path(comparison.folder, paste0("measure=",measure,"-sumsq.csv")))
        colnames(sumsq) = paste0(measure,",",colnames(sumsq))
        all.sumsq = cbind(all.sumsq, sumsq)
    }
    write.csv(x=all.coeffs, file=file.path(comparison.folder, "all-coeffs-by-measure.csv"))
    write.csv(x=all.sumsq, file=file.path(comparison.folder, "all-sumsq-by-measure.csv"))


	# ==================================================================================================
	## handling the results by transformation type: writing into files 
	
#    all.coeffs = c()
#    all.sumsq = c()
#    for(transf.type in transf.types){
#        result = compare.measures.by.transf(comp.measures, transf.type, data.frame.folder, raw.results.folder)
#        coeffs = result$coeffs
#        write.csv(x=coeffs, file=file.path(comparison.folder, paste0("transf=",transf.type,"-coeffs.csv")))
#        colnames(coeffs) = paste0(transf.type,",",colnames(coeffs))
#        all.coeffs = cbind(all.coeffs, coeffs)
#
#        sumsq = result$sumsq
#        write.csv(x=sumsq, file=file.path(comparison.folder, paste0("transf=",transf.type,"-sumsq.csv")))
#        colnames(sumsq) = paste0(transf.type,",",colnames(sumsq))
#        all.sumsq = cbind(all.sumsq, sumsq)
#    }
#    
#    write.csv(x=all.coeffs, file=file.path(comparison.folder, "all-coeffs-by-transf.csv"))
#    write.csv(x=all.sumsq, file=file.path(comparison.folder, "all-sumsq-by-transf.csv"))
# ==================================================================================================

 
}
