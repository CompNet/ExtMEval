


library(latex2exp)


###############################################################################################
# It generated the results of the relative importance analysis, for the considered measures. 
#	The order of the terms in each bar is shown in the legend.
#	The relative importance scores represented on the y -axis are square-roots
#
# transf.types: a vector of values regarding transformation types
# comp.measures: a vector of values regarding external evaluation measures
###############################################################################################
plot.all.relaimp = function(transf.types, comp.measures)
{
    tlog("compute all relaimp")

    #plot.format = PLOT.AS.PDF
    plot.format = "" # no individual plot

	data.frame.folder = DATA.FRAME.FOLDER
    tlog(4, "data frame folder is: ", data.frame.folder)
    if(!dir.exists(data.frame.folder))
        return(0)

    raw.results.folder = REGRESSION.RAW.RES.FOLDER
	comparison.folder = REGRESSION.COMP.RES.FOLDER
	
    plot.folder = REL.IMP.ANALYSIS.FOLDER
    if(!dir.exists(plot.folder))
        dir.create(plot.folder, recursive=TRUE)

	
	# ==================================================================================================
	## plot by measure
	
	# =============================================
	# max y value estimation
	max.y.value = 0
	for(i in 1:length(comp.measures)){
		measure = comp.measures[i]
		a = read.csv(file=file.path(comparison.folder, paste0("measure=",measure,"-coeffs.csv")), header=TRUE, check.names=FALSE)
		m = a[-1,-1] # remove first row and first column
		m = sqrt(m^2) # this makes Inf for 0 values
		
		if(max(apply(m,2,sum))>max.y.value) # column-wise summation
			max.y.value = max(apply(m,2,sum))
	}
	max.y.value = round(max.y.value + 0.05,1) # e.g. if it equal '0.39', we get '0.4'
	# =============================================
	
	nrow = 2
	ncol = (length(comp.measures)+1)/nrow
     
    pdf(file=file.path(plot.folder, "by-measure-prop-norm.pdf"), compress=FALSE, width=4*ncol, height=4*nrow) # for k_init=3

	par(mfrow = c(nrow, ncol)) # 2-by-2 grid of plots
	par(oma = c(2, 1, 0, 1)) # make room (i.e. the 4's) for the overall x and y axis titles
	par(mar = c(3, 4, 3, 2)) # make the plots be closer together

    global.measure.results = list()
    for(i in 1:length(comp.measures)){
        measure = comp.measures[i]
        print(measure)
		
		enable.legend = FALSE
		if(i == length(comp.measures)) # put the legend only once
			enable.legend = TRUE

        a = read.csv(file=file.path(comparison.folder, paste0("measure=",measure,"-coeffs.csv")), header=TRUE, check.names=FALSE)
        
        frow = unlist(a[1,-1]) # firdst row
        global.measure.results[[measure]] = sqrt(frow^2) # first row contains data regarding transformations at global level (without 'n', 'k', etc.)
        m = a[-1,-1] # remove first row and first column

        m = sqrt(m^2) # this makes Inf for 0 values
        #print(m)
		#print("----------")
		
		# ---------------------------------
        
        vector.list = as.list(m)
        vector.list = lapply(vector.list, function(vec) { indx = which(is.infinite(vec) | is.nan(vec)); vec[indx]=0; return(vec) } )
        #print(vector.list)
        
        # p.filename = paste0("measure=",measure,"-sumsq-plot.pdf")
        p.filename = NA
        title = paste0("measure=",measure)
        
        vec.descs = names(vector.list) # contains transf types
        vec.descs = gsub(" ","\n",vec.descs)
        make.stacked.bar.plot(vector.list, vec.descs, a[-1,1], space=0.25, ylabel=TeX("\\sqrt{SRC}"), vertical.xlabel=FALSE, ylimit=c(0,max.y.value),
		plot.folder=plot.folder, plot.filename=p.filename, plot.format=plot.format, title=title, enable.legend=enable.legend, legend.title="Variables")
    }

   dev.off()




# ==================================================================================================
## plot by transformation type

#	nrow = 2
#	ncol = (length(transf.types)+1)/nrow
#    
#    pdf(file=file.path(plot.folder, "by-transf.pdf"), compress=FALSE, width=4*ncol, height=4*nrow) # for k_init=3
#
#	par(mfrow = c(nrow, ncol)) # 2-by-2 grid of plots
#	par(oma = c(2, 1, 0, 1)) # make room (i.e. the 4's) for the overall x and y axis titles
#	par(mar = c(3, 4, 3, 2)) # make the plots be closer together
#
#    enable.legend = FALSE
#    global.transf.results = list()
#    for(transf.type in transf.types){
#        print("-*-*--*-***---*-*-*-**-*-*----**")
#        print(transf.type)
#
#        a = read.csv(file=file.path(comparison.folder, paste0("transf=",transf.type,"-sumsq.csv")), header=TRUE, check.names=FALSE)
#        #a = a[,comp.measures]
#        print(a)
#        frow = unlist(a[1,-1])
#        global.transf.results[[transf.type]] = sqrt(frow) # first row contains data regarding measures at global level (without 'n', 'k', etc.)
#        m = a[-1,-1] # remove first row and first column
#
#        # the min value can be 0.001, since 0.001*100=1 and log2(1)=0, we multiply by 200. Hence, for min value we get log2(2)=1
#        m = sqrt(m) # this makes Inf for 0 values
#    
#        print("-----")
#        vector.list = as.list(m)
#        vector.list = lapply(vector.list, function(vec) { indx = which(is.infinite(vec)); vec[indx]=0; return(vec) } )
#        print(vector.list)
#
#        p.filename = paste0("transf=",transf.type,"-sumsq-plot.pdf")
#        title = paste0("transf=",transf.type)
#        
#        vec.descs = names(vector.list) # contains measures
#        vec.descs = gsub(" ","\n",vec.descs)
#
#        make.stacked.bar.plot(vector.list, vec.descs, a[-1,1], ylabel="sqrt(Importance)", vertical.xlabel=FALSE, ylimit=c(0,35),
#		plot.folder=plot.folder, plot.filename=p.filename, plot.format=plot.format, title=title, enable.legend=enable.legend, legend.title="Variables")
#    }
#
#
#    dev.off()

}
