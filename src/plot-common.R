
# we have a color list: distinct color list (that we can distinguish clearly). There are 14 colors. This is main color list

palette <- as.list(rainbow(14))
palette[[1]] = "orange"
palette[2:5] = rainbow(4, v=0.7)
palette[[6]] = "maroon1" #
palette[[7]] = "plum1" # 
palette[[8]] = "red" #
palette[[9]] = "seagreen4" #
palette[[10]] = "black" # 
palette[[11]] = "burlywood4" # 
palette[[12]] = "yellow" #
palette[[13]] = "lightseagreen"
palette[[14]] = "gray87" # light gray





# ---------------------------------------------







# source of bar plots: https://www.statmethods.net/graphs/bar.html


## an example of stacked bar plot
## a (and b) is a vector of size 100, contains imb optimality values
## convert opt values into 3 categories: "x<90", "90<=x<100", "x=100"
## each x axis value corresponds to a network
## y axis corresponds to optimality frequency
#a = c(2, 2, 1, 1, 2, 0, 2, 1, 1, 2) # network 1
#b = c(2, 1, 1, 1, 2, 2, 1, 2, 0, 1) # network 2
#counts = cbind(table(a), table(b))
## 				network1 network2
## x<90             1        1
## 90<=x<100        4        5
## x=100            5        4
#rownames(counts) = c("x<90", "90<=x<100", "x=100")
#colnames(counts) = c("network1", "network2")
## colors for row names
#barplot(counts, col=c("red", "orange", "darkblue"), legend=rownames(counts))

# an example of grouped bar plot
# use the previous example, just set the following parameter to TRUE: 'beside'
# barplot(counts, col=c("red", "orange", "darkblue"), legend=rownames(counts), beside=TRUE)








################################################################
# 
# count.levels can be provided to use always the same levels in layout plots
# combine.strategy: "rbind" or "cbind"
################################################################
make.bar.plot = function(vector.list, vector.descs, ctgry.names, space=1, is.beside=FALSE, ylabel="", ylimit=NA, vertical.xlabel=TRUE,
		plot.folder=NA, plot.filename=NA, plot.format="", title, enable.legend=TRUE, legend.title=NA)
{

    colors = unlist(palette[1:length(ctgry.names)])
	colors <- adjustcolor(colors, alpha.f = 0.7)

    m = do.call(cbind, vector.list)


    rownames(m) = ctgry.names
    colnames(m) = vector.descs


    las.val = 1
    if(vertical.xlabel)
        las.val = 2

	if(plot.format == PLOT.AS.PDF){
		if(!dir.exists(plot.folder))
			dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		pdf(file=file.path(plot.folder, plot.filename), compress=FALSE)
	}


    if(enable.legend){
		# Add extra space to right of plot area; change clipping to figure
		par(mar=c(2, 2, 2, 6), xpd=TRUE)
        barplot(m, ylim=ylimit, space=space, col=colors, beside=is.beside, las=las.val, ylab=ylabel, main=title, border="white",
             legend.text=TRUE, args.legend=list(title=legend.title, x="topright", inset=c(-0.15,0), cex=1), cex.names=1) 
    } else {
        barplot(m, ylim=ylimit, space=space, col=colors, beside=is.beside, las=las.val, ylab=ylabel, main=title, border="white", cex.names=0.75) # cex.names=0.45, cex.lab=0.75, cex.axis=0.75
    }

    if(plot.format == PLOT.AS.PDF)
        dev.off()
}



################################################################
# 
################################################################
make.stacked.bar.plot = function(vector.list, vector.descs, ctgry.names, space=1, ylabel="", ylimit=NULL, vertical.xlabel=TRUE,
		plot.folder=NA, plot.filename=NA, plot.format="", title="", enable.legend=TRUE, legend.title=NA){
#	test
#	a = c(2, 20, 3, 9)
#	b = c(6, 2, 13, 19)
#   m = do.call(cbind, list(a=a, b=b))
#	barplot(m, ylim=, col=c("red","green","yellow","blue"), beside=FALSE, las=2, main="dsf", border=NA) # omit bar border

	make.bar.plot(vector.list, vector.descs, ctgry.names, space=space, is.beside=FALSE, ylabel=ylabel, ylimit=ylimit, vertical.xlabel=vertical.xlabel,
		plot.folder=plot.folder, plot.filename=plot.filename, plot.format=plot.format, title=title, enable.legend=enable.legend, legend.title=legend.title)

}



################################################################
# Warning: it is not generic: it depends on the number of bar that will be side by side, i.e. nb.bar
################################################################
make.stacked.bar.plot.with.group = function(vector.list1, vector.list2,  vector.descs, ctgry.names, ylabel="", ylimit=NULL, vertical.xlabel=TRUE,
		plot.folder=NA, plot.filename=NA, plot.format="", title="", enable.legend=TRUE, legend.title=NA){
#	test
#	a = c(2, 20, 3, 9)
#	b = c(6, 2, 13, 19)
#   m = do.call(cbind, list(a=a, b=b))
#	barplot(m, ylim=, col=c("red","green","yellow","blue"), beside=FALSE, las=2, main="dsf", border=NA) # omit bar border




    nb.group = length(vector.descs)
    print(ctgry.names)
    nb.bar = 2
    in.space = rep(0,nb.bar-1)
    betw.space = 1
    first.space=0
    spaces = c(first.space,rep(c(in.space,betw.space),nb.group-1),in.space)

    vector.descs = rep(vector.descs, each=2)

    vector.list = list()
    count = 0
    for(i in 1:length(vector.list1)){
        count = count + 1
        vector.list[[count]] = vector.list1[[i]]
        count = count + 1
        vector.list[[count]] = vector.list2[[i]]
    }
	
    make.bar.plot(vector.list, vector.descs, ctgry.names, space=spaces, is.beside=FALSE, ylabel=ylabel, ylimit=ylimit, vertical.xlabel=vertical.xlabel,
		plot.folder=plot.folder, plot.filename=plot.filename, plot.format=plot.format, title, enable.legend=enable.legend, legend.title=legend.title)
}











