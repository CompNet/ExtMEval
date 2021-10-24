
###############################################################################################
# It generates grid plot with 'nrow' rows and 'ncol' columns. In the end, there are nrow x ncols subplots.
# Each subplot shows the evolution of measure scores in function of a single parameter. The other parameters are fixed to some values.
#
###############################################################################################
plot.evol.generic = function(df, plot.folder, measure, transf.type, info.fixed.dim, evol.dim.desc, first.dim.desc, first.dim.vals, second.dim.desc, second.dim.vals, ylimit=c(0,1)){
	ncol = length(first.dim.vals)
	nrow = length(second.dim.vals)
	
	print("----")
	print(evol.dim.desc)
	print(paste0(transf.type,", ",measure))
	pdf(file=file.path(plot.folder, paste0("evol-",evol.dim.desc,"_", info.fixed.dim, "_", transf.type,"-",measure,".pdf")), compress=FALSE, width=2.25*ncol, height=2*nrow)
	
	par(mfrow = c(nrow, ncol)) # 2-by-2 grid of plots
	par(oma = c(4, 4, 5, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
	par(mar = c(4, 3, 2.5, 1)) # make the plots be closer together
	
	for(f in first.dim.vals){
		for(s in second.dim.vals){
			indx = which(df[,first.dim.desc] == f & df[,second.dim.desc] == s)
			desc = paste0(first.dim.desc,": ", f, ", ",second.dim.desc,": ", s)
			plot(x=df[indx,evol.dim.desc],y=df[indx,"score"],ylim=ylimit, main=desc, type="o", col="blue", xlab=NA, ylab=NA)
		}
	}
	
	plot.title = paste0(transf.type,", ",measure," (",info.fixed.dim,")")
	title(plot.title, line = 0, outer = TRUE)
	# print the overall labels
	mtext(paste0("x axis: ", evol.dim.desc), side = 1, outer = TRUE, line = 2) # x axis label
	mtext(paste0("y axis: ", "score"), side = 2, outer = TRUE, line = 2) # y axis label
	dev.off()
}



###############################################################################################
#	It is the starting method for generating grid plots in order to observe the evolution of measure scores.
#	This method needs to reduce the number of dimensions from 4 to 3. 
#	We opt for doing this by fixing: 1) n or 2) h. We chose them, because They do not have any substantial effect on the measures scores in general.
#	We fix these parameters with respect to the considered n and h values in the main.
#
# n.vals: a vector of values regarding number of nodes
# k.vals: a vector of values regarding number of clusters
# h.vals: a vector of values regarding cluster size heterogeneity
# transf.types: a vector of values regarding transformation types
# subset.props: a vector of values regarding transformation intensity
# comp.measures: a vector of values regarding external evaluation measures
#
###############################################################################################
plot.evol.stats = function(n.vals, k.vals, h.vals, transf.types, subset.props, comp.measures)
{
	# because of the strange behavior of read.csv towards numerals, convert them into string
	subset.props = as.character(subset.props)
	h.vals = as.character(h.vals)
	
	# ---------------------------
	# to fix 'n' to some value to reduce from 4 dimensions to 3 dimensions
	ARBITARY.N.VAL = n.vals[order(n.vals)][length(n.vals)/2] # we pick a medium value
	# ARBITARY.N.VAL = 6480
			
	# to fix 'h' to some value to reduce from 4 dimensions to 3 dimensions
	ARBITARY.H.VAL = h.vals[order(h.vals)][length(h.vals)/2] # we pick a medium value
	#ARBITARY.H.VAL = 0.5
	# ---------------------------
	
	data.frame.folder = DATA.FRAME.FOLDER
	tlog(4, "data frame folder is: ", data.frame.folder)
	if(!dir.exists(data.frame.folder))
		return(0)
	
	dir.create(VISUAL.INSPECTION.FOLDER, recursive=TRUE)
	
	for(transf.type in transf.types){
		for(measure in comp.measures){
			df = read.csv(file.path(data.frame.folder, paste0(measure,"-data-frame.csv")),sep=";", stringsAsFactors=F, check.names=F, header=T)
			df[,"p"] = as.character(df[,"p"])
			df[,"h"] = as.character(df[,"h"])
			indx = which(df$t == transf.type)
			df2 = df[indx,]
			
			if(measure == "VI"){
				df2[,"score"] = df2[,"score"]/log(ARBITARY.N.VAL)
			}
			
			# n fixed
			indx = which(df2$n == ARBITARY.N.VAL)
			info.n = paste0("n=",ARBITARY.N.VAL)
			plot.folder = file.path(VISUAL.INSPECTION.FOLDER,measure,info.n)
			dir.create(plot.folder, recursive=TRUE, showWarnings=FALSE)
			plot.evol.generic(df2[indx,], plot.folder, measure, transf.type, info.n, evol.dim.desc="k", first.dim.desc="h", first.dim.vals=h.vals, second.dim.desc="p", second.dim.vals=subset.props, ylimit=c(0,1))
			plot.evol.generic(df2[indx,], plot.folder, measure, transf.type, info.n, evol.dim.desc="p", first.dim.desc="h", first.dim.vals=h.vals, second.dim.desc="k", second.dim.vals=k.vals, ylimit=c(0,1))
			plot.evol.generic(df2[indx,], plot.folder, measure, transf.type, info.n, evol.dim.desc="h", first.dim.desc="k", first.dim.vals=k.vals, second.dim.desc="p", second.dim.vals=subset.props, ylimit=c(0,1))
			
			
			
			if(measure == VI){
				df2[,"score"] = df2[,"score"]*log(ARBITARY.N.VAL)
			}
			
			# h fixed
			indx = which(df2$h == ARBITARY.H.VAL)
			info.h = paste0("h=",ARBITARY.H.VAL)
			plot.folder = file.path(VISUAL.INSPECTION.FOLDER,measure,info.h)
			dir.create(plot.folder, recursive=TRUE, showWarnings=FALSE)
			plot.evol.generic(df2[indx,], plot.folder, measure, transf.type, info.h, evol.dim.desc="k", first.dim.desc="n", first.dim.vals=n.vals, second.dim.desc="p", second.dim.vals=subset.props, ylimit=NULL)
			plot.evol.generic(df2[indx,], plot.folder, measure, transf.type, info.h, evol.dim.desc="p", first.dim.desc="n", first.dim.vals=n.vals, second.dim.desc="k", second.dim.vals=k.vals, ylimit=NULL)
			plot.evol.generic(df2[indx,], plot.folder, measure, transf.type, info.h, evol.dim.desc="n", first.dim.desc="k", first.dim.vals=k.vals, second.dim.desc="p", second.dim.vals=subset.props, ylimit=NULL)
			
			
		}
	}
	
}

