
library('plot.matrix')
# https://cran.r-project.org/web/packages/plot.matrix/vignettes/plot.matrix.html#move-the-key


###############################################################################################
# Significance of the results regarding the comparison of the segment heights performed in the relative importance analysis over:
#	- all pairs of measures, considered for each transformation and parameter set.
#
# Although we have already applied linear regression in 'src/relaimp-analysis.R', here we need to reapply again.
# Because, in regression, we need to select a reference variable, from which p-values are computed for signifance analysis.
#	Since we want to do that for each pair of measures, we need to run as many regressions as the number of transformations.
#	In each regression we use a different measure as reference variable.
#
# Note that we do not keep the results files.
#
# measures: considered evaluation measures
# transf.type: considered transformation type
# data.frame.folder: the folder path, where the data frame csv files are located
# raw.results.folder: the folder path, where raw regression results (produced by this method) will be located.
#					Actually, it is useless, since we do not keep the results
#
###############################################################################################
compare.measures.by.transf.type.for.significance = function(measures, transf.type, data.frame.folder, raw.results.folder){
    print("compare transfs by measure for significance")
    
    # prepare regression
    thedata = c()
    for(j in 1:length(measures)){
        #for(j in 1:1){
        print("========**********========")
        print(measures[j])
        
        fpath = file.path(data.frame.folder, paste0(measures[j],"-data-frame.csv"))
        print(fpath)
        df1 <- read.csv(fpath, sep=";", stringsAsFactors=F, check.names=F, header=T)
        indx = which(df1$t == transf.type)
        df1 = df1[indx,]
        thedata = rbind(thedata, data.frame(score=df1[,"score"],k=df1[,"k"],p=df1[,"p"],h=df1[,"h"],n=df1[,"n"], m=measures[j]))
    }
    thedata[,"m"] = as.factor(thedata[,"m"])
    
    
    simpl.measures =  gsub("[^[:alnum:]]", " ", measures) # remove special characters
    simpl.measures = tolower(gsub(" ", "", simpl.measures)) # remove whitespaces
    
    ord = order(simpl.measures) # order measures, since factor() will order them alphabetically
    simpl.measures = simpl.measures[ord]
    measures = measures[ord]
    
    
    
    #thedata[,"n"] =thedata[,"n"]-mean(thedata[,"n"])
    thedata[,"n"] = scale(thedata[,"n"])
    #thedata[,"k"] = thedata[,"k"]-mean(thedata[,"k"])
    thedata[,"k"] = scale(thedata[,"k"])
    #thedata[,"p"] = thedata[,"p"]-mean(thedata[,"p"])
    thedata[,"p"] = scale(thedata[,"p"])
    #thedata[,"h"] = thedata[,"h"]-mean(thedata[,"h"])
    thedata[,"h"] = scale(thedata[,"h"])
    
    
    # prepare output data
    results = list()
    nb.measures = length(measures)
    print(nb.measures)
    
    for(desc in c("n","k","h","p")){
        m = matrix(NA,nrow=nb.measures,ncol=nb.measures)
        rownames(m) = measures
        colnames(m) = measures
        results[[desc]] = m
    }
    
	# run regressions
	# Note that we need to run regressions again, 
	# 	because each time we will change the reference variable in order to capture the p-value between all pair of parameters	
	
    # transf.types = levels(df$m)
    for(ref.no in 1:length(levels(thedata$m))){
        thedata <- within(thedata, m <- relevel(m, ref = ref.no)) # ref.no respects the order in levels(df$t)
        print(ref.no)
        ref.measure = measures[ref.no]
        print(ref.measure)
        
        
        cat.var = c("m")
        singles = c(cat.var,"n","k","h","p")
        
        pairs = c()
        for(d in c("n","k","h","p")){
            pairs = c(pairs, paste0(cat.var, ":", d))
        }
        
        my.formula = as.formula(paste0("score ~ ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + ") ))
        #my.formula = as.formula(paste0("score ~ ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + "), " + ",  paste0(triples, collapse=" + ")))
        #print(my.formula)
		options("width"=200) # the extraction process supposes that all the columns are in the same horizontal level, i.e. each line contains all the information regarding a specific parameter
        reg = lm(my.formula, thedata) # the first single term as ref variable, so omitted ==>> coeff of the reg model
        
        print("done for model reg -------------------")
        
        sumreg = summary(reg)
        raw.result.file.path = file.path(raw.results.folder, paste0("summary-reg_","compare-measures-by-transf=",transf.type,".txt"))
        sink(raw.result.file.path)
        print(sumreg)
        sink()  # returns output to the console
        
        
		# ================================================================
		# Extraction of signifance results from regression result
		# Note taht the extraction process supposes that all the columns are in the same horizontal level,
		# i.e. each line contains all the information regarding a specific parameter.
		# Therefore, if there is an asterisk in a given line, then the corresponding parameter is statistically significant
	
        con <- file(raw.result.file.path, "r")
        lines <- readLines(con)
        close(con)
        
        process = FALSE
        significance.results = c()
        for(line in lines){
            if(line == "---")
                process = FALSE
            
            if(process){
                result = grepl("\\*",line)
                significance.results = c(significance.results, result)
            }
            
            if(grepl("Intercept",line))
                process = TRUE
        }
		# ================================================================
        
        unlink(raw.result.file.path) # we do not keep the resulting files
        
        terms = names(reg$coefficients)[-1] # remove the intercept
        # print(terms)
        # print(significance.results)
        
        # process signif results
        for(i in 1:length(terms)){
            term = terms[i]
            if(grepl(":",term)){
                parts = unlist(strsplit(term, ":"))
                desc = parts[2]
                target.measure = substring(parts[1], 2)
                results[[desc]][ref.measure,target.measure] = significance.results[i]
            }
        }
        
    }
    
    return(results)  
}






###############################################################################################
# Significance of the results regarding the comparison of the segment heights performed in the relative importance analysis over:
#	- all pairs of transformations, considered for each measure and parameter set.
#
# Although we have already applied linear regression in 'src/relaimp-analysis.R', here we need to reapply again.
# Because, in regression, we need to select a reference variable, from which p-values are computed for signifance analysis.
#	Since we want to do that for each pair of transformations, we need to run as many regressions as the number of transformations.
#	In each regression we use a different transformation type as reference variable.
#
# Note that we do not keep the results files.
#
# measure: evaluation measure
# data.frame.folder: the folder path, where the data frame csv files are located
# raw.results.folder: the folder path, where raw regression results (produced by this method) will be located.
#					Actually, it is useless, since we do not keep the results
# (in the input parameters, we do not need consider transformation types, since we have this information from data frame csv files)
#
###############################################################################################
compare.transfs.by.measure.for.significance = function(measure, data.frame.folder, raw.results.folder){
    
    print("compare transfs by measure for significance")
    
    # prepare regression
    df<-read.csv(file.path(data.frame.folder, paste0(measure,"-data-frame.csv")), sep=";", stringsAsFactors=F, check.names=F, header=T)
    orig.transf.types = unique(df$t)
    df[,"t"] = gsub(" ", "", df[,"t"]) # remove whitespace
    indx = which(df[,"t"] == "1NewCluster")
    df[indx,"t"] = "OneNewCluster"
    modif.transf.types = unique(df[,"t"])
    
    assoc.modif.to.orig.for.transf.types = as.list(orig.transf.types)
    names(assoc.modif.to.orig.for.transf.types) = modif.transf.types
    
    # print(modif.transf.types)
    df$t = as.factor(df$t)
    
    thedata = data.frame(cbind(df[,c("score","n","k","h","p","t")]))
    #colnames(thedata) = c("score","n","k","h","p", levels(df$t))
    
    thedata[,"n"] =thedata[,"n"]-mean(thedata[,"n"])
    #thedata[,"n"] = scale(thedata[,"n"])
    thedata[,"k"] = thedata[,"k"]-mean(thedata[,"k"])
    #thedata[,"k"] = scale(thedata[,"k"])
    thedata[,"p"] = thedata[,"p"]-mean(thedata[,"p"])
    #thedata[,"p"] = scale(thedata[,"p"])
    thedata[,"h"] = thedata[,"h"]-mean(thedata[,"h"])
    #thedata[,"h"] = scale(thedata[,"h"])
    

        
    # prepare output data
    results = list()
    nb.transf.types = length(modif.transf.types)
    print(nb.transf.types)
    
    for(desc in c("n","k","h","p")){
        m = matrix(NA,nrow=nb.transf.types,ncol=nb.transf.types)
        rownames(m) = modif.transf.types
        colnames(m) = modif.transf.types
        results[[desc]] = m
    }
    print(modif.transf.types)
    
    
    # run regressions
	# Note that we need to run regressions again, 
	# 	because each time we will change the reference variable in order to capture the p-value between all pair of parameters	
    
    # transf.types = levels(df$t)
    for(ref.no in 1:length(levels(df$t))){
        thedata <- within(thedata, t <- relevel(t, ref = ref.no)) # ref.no respects the order in levels(df$t)
        ref.transf.type = levels(df$t)[ref.no]
        
        cat.var = c("t")
        singles = c(cat.var,"n","k","h","p")
        
        pairs = c()
        for(d in c("n","k","h","p")){
            pairs = c(pairs, paste0(cat.var, ":", d))
        }
        
        my.formula = as.formula(paste0("score ~ ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + ") ))
        #my.formula = as.formula(paste0("score ~ ", paste0(singles, collapse=" + "), " + ",  paste0(pairs, collapse=" + "), " + ",  paste0(triples, collapse=" + ")))
        #print(my.formula)
		options("width"=200) # the extraction process supposes that all the columns are in the same horizontal level, i.e. each line contains all the information regarding a specific parameter
		reg = lm(my.formula, thedata) # the first single term as ref variable, so omitted ==>> coeff of the reg model
        
        print("done for model reg -------------------")
        
        sumreg = summary(reg)
        raw.result.file.path = file.path(raw.results.folder, paste0("summary-reg_","compare-transfs-by-measure=",measure,".txt"))
		print(raw.result.file.path)
		
        sink(file=raw.result.file.path)
        print(sumreg)
        sink(NULL)  # returns output to the console
        
        
		# ================================================================
		# Extraction of signifance results from regression result
        # Note taht the extraction process supposes that all the columns are in the same horizontal level,
		# i.e. each line contains all the information regarding a specific parameter.
		# Therefore, if there is an asterisk in a given line, then the corresponding parameter is statistically significant
	
		con <- file(raw.result.file.path, "r")
        lines <- readLines(con)
        close(con)
        
        process = FALSE
        significance.results = c()
        for(line in lines){
            if(line == "---")
                process = FALSE
            
            if(process){
                result = grepl("\\*",line)
                significance.results = c(significance.results, result)
            }
            
            if(grepl("Intercept",line))
                process = TRUE
        }
		# ================================================================
		
        unlink(raw.result.file.path) # we do not keep the resulting files
        
        terms = names(reg$coefficients)[-1] # remove the intercept
        # print(terms)
        # print(significance.results)
        
        # process signif results
        for(i in 1:length(terms)){
            term = terms[i]
            if(grepl(":",term)){
                parts = unlist(strsplit(term, ":"))
                desc = parts[2]
                target.transf.type = substring(parts[1], 2)
                results[[desc]][ref.transf.type,target.transf.type] = significance.results[i]
            }
        }
        
    }
    
    # change row and column names in the matrices
    for(i in 1:length(results)){
        rownames(results[[i]]) = sapply(rownames(results[[i]]), function(rowname) assoc.modif.to.orig.for.transf.types[[rowname]])
        colnames(results[[i]]) = sapply(colnames(results[[i]]), function(colname) assoc.modif.to.orig.for.transf.types[[colname]])
    }
    
    return(results)    
}







###############################################################################################
# Significance of the results regarding the comparison of the segment heights performed in the relative importance analysis over:
#	- all pairs of transformations, considered for each measure and parameter set.
#	- all pairs of measures, considered for each transformation and parameter set.
#
# transf.types: a vector of values regarding transformation types
# comp.measures: a vector of values regarding external evaluation measures
###############################################################################################
perform.significance.analysis = function(transf.types, comp.measures)
{
    tlog("perform significance analysis")
    
    data.frame.folder = DATA.FRAME.FOLDER
    tlog(4, "data frame folder is: ", data.frame.folder)
    if(!dir.exists(data.frame.folder))
        return(0)
    
	plot.folder = SIGNIFICANCE.ANALYSIS.FOLDER
	if(!dir.exists(plot.folder))
		dir.create(plot.folder, recursive=TRUE)    

	
	# ==========================================================================================
	# Significance of the results regarding the comparison of the segment heights performed
	#	in the relative importance analysis over
	#	 all pairs of transformations, considered for each measure and parameter set.
	#	(each row corresponds to a specific measure, and each column corresponds to a specific parameter)
	# ==========================================================================================
	
    graph.name = "significance_results_for_measures"
    frmt = "pdf"
    if(!is.na(frmt)){
        nrow = length(comp.measures)
        ncol = length(c("n","k","h","p"))

        plot.filename = paste0(graph.name,".PDF")
        pdf(file=file.path(plot.folder, plot.filename), compress=FALSE, width=4*ncol, height=2*nrow)

        par(mfrow = c(nrow, ncol)) # 2-by-2 grid of plots
        par(oma = c(0,6,6,0)) # make room (i.e. the 4's) for the overall x and y axis titles
        par(mar = c(2.5,3, 3, 1)+0.1) # make the plots be closer together
    }

	counter=0
    for(measure in comp.measures){
		counter=counter+1
        matrix.list = compare.transfs.by.measure.for.significance(measure, data.frame.folder, plot.folder)
		
		counter2=0
        for(desc in names(matrix.list)){
			counter2=counter2+1
            m = matrix.list[[desc]]
            # change row and column names in the matrix
            rownames(m) = sapply(rownames(m), function(rowname) ABBR.TRANSF.TYPES[[rowname]])
            colnames(m) = sapply(colnames(m), function(colname) ABBR.TRANSF.TYPES[[colname]])

            plot(m, border=TRUE, reorder=FALSE, key=NULL, #key=list(side=3, cex.axis=0.75),
                 breaks=c(TRUE, FALSE), main=desc, col=c('green','red'), las=1, ann=FALSE)
		 	if(counter2==1)
		 		mtext(measure, side=2, line=2, at=grconvertY(0.5,'npc','nic'), outer=TRUE)
			if(counter==1)
				mtext(desc, side=3, line=2, at=grconvertX(0.5,'npc','nic'), outer=TRUE)
			
        }
    }

    if(!is.na(frmt)){
        dev.off()
    }
    
    
	
	
	# ==========================================================================================
	# Significance of the results regarding the comparison of the segment heights performed
	#	in the relative importance analysis over
	#	 all pairs of measures, considered for each transformation and parameter set.
	#	(each row corresponds to a specific trasnformation type, and each column corresponds to a specific parameter)
	# ==========================================================================================
    
	 plot.folder = SIGNIFICANCE.ANALYSIS.FOLDER
	 if(!dir.exists(plot.folder))
		 dir.create(plot.folder, recursive=TRUE)
	 
     graph.name = "significance_results_for_transfs"
     frmt = "pdf"
     if(!is.na(frmt)){
         nrow = length(transf.types)
         ncol = length(c("n","k","h","p"))
         
         plot.filename = paste0(graph.name,".PDF")
         pdf(file=file.path(plot.folder, plot.filename), compress=FALSE, width=4*ncol, height=2*nrow)
         
         par(mfrow = c(nrow, ncol)) # 2-by-2 grid of plots
         par(oma = c(0,6,6,0)) # make room (i.e. the 4's) for the overall x and y axis titles
         par(mar = c(2.5,3, 3, 1)+0.1) # make the plots be closer together
     }
     
	 counter=0
     for(transf.type in transf.types){
		 counter=counter+1
         matrix.list = compare.measures.by.transf.type.for.significance(comp.measures, transf.type, data.frame.folder, plot.folder)
		 
		 counter2=0
         for(desc in names(matrix.list)){
			 counter2=counter2+1
             m = matrix.list[[desc]]
             plot(m, border=TRUE, reorder=FALSE, key=NULL, #key=list(side=3, cex.axis=0.75), 
                  breaks=c(TRUE, FALSE), main=desc, col=c('green','red'), las=1, ann=FALSE)
		  	if(counter2==1)
		  		mtext(transf.type, side=2, line=2, at=grconvertY(0.5,'npc','nic'), outer=TRUE)
			if(counter==1)
				mtext(desc, side=3, line=2, at=grconvertX(0.5,'npc','nic'), outer=TRUE)
			
         }
     }
     
     if(!is.na(frmt)){
         dev.off()
     }
    
    
}