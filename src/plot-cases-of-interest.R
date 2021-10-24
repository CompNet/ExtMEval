
dir.create(VISUAL.INSPECTION.FOLDER, recursive=TRUE)


###############################################################################################
# RI score as a function of k, for the 1 New Cluster transformation, and for several values of p
###############################################################################################

	plot.folder = VISUAL.INSPECTION.FOLDER
	t = TRANSF.1NEWCLU
    #t = TRANSF.NEIG.CLU.SWAP
    #t = TRANSF.SINGLCLU
    measure = SIM.MEASURES["RI"]

    df<-read.csv(file.path(DATA.FRAME.FOLDER, paste0(measure,"-data-frame.csv")), sep=";", stringsAsFactors=F, check.names=F, header=T)


    indx1 = (df[,"t"] == t)
    # indx2 = (df[,"h"] == 0)
    # indx3 = (df[,"n"] == 6480)
	indx2 = (df[,"h"] == H.VALS[order(H.VALS)][1]) # the first value among the considered ones
	indx3 = (df[,"n"] == N.VALS[order(N.VALS)][length(N.VALS)/2]) # we pick a medium value
    indx = which(indx1 & indx2 & indx3)
    df2 = df[indx,]

    pdf(file=file.path(plot.folder,paste0("evol-k_",measure,"_",t,".pdf")))
    # evol k
    plot(NULL, xlim=c(min(K.VALS),max(K.VALS)), ylim=c(0,1), xlab="k", ylab=measure, main=paste0("Evolution of ",measure," by k\nn=6480, h=0\nt=",t),cex.main=1.5, cex.axis=1.2, cex.lab=1.2)
    p.vals = SUBSET.PROPS
    for(i in 1:length(p.vals)){
        p = p.vals[i]
        print(p)
        indx4 = which(df2[,"p"] == as.character(p))
        print(indx4)
        df3 = df2[indx4,]
        lines(x=df3[,"k"], y=df3[,"score"])
        posx = K.VALS[order(K.VALS)][length(K.VALS)/2] # a medium value
        text(posx, df3[nrow(df3)-1,"score"],  paste("p=",p),
             cex=1.2, pos=3,col="red")
    }
    dev.off()



################################################################################################
## 
################################################################################################
#
#	plot.folder = VISUAL.INSPECTION.FOLDER
#	t = TRANSF.ORT.CLU
#    #t = TRANSF.NEIG.CLU.SWAP
#    #t = TRANSF.SINGLCLU
#    measure = SIM.MEASURES["HA.ARI"]
#
#    df<-read.csv(file.path(DATA.FRAME.FOLDER, paste0(measure,"-data-frame.csv")), sep=";", stringsAsFactors=F, check.names=F, header=T)
#
#
#    indx1 = (df[,"t"] == t)
#    indx2 = (df[,"h"] == 0)
#    indx3 = (df[,"n"] == 6480)
#    indx = which(indx1 & indx2 & indx3)
#    df2 = df[indx,]
#
#    pdf(file=file.path(plot.folder,paste0("evol-k_",measure,"_",t,".pdf")))
#    # evol k
#    plot(NULL, xlim=c(2,11), ylim=c(0,1), xlab="k", ylab=measure, main=paste0("Evolution of ",measure," by k\nn=6480, h=0\nt=",t), cex.main=1.5, cex.axis=1.2, cex.lab=1.2)
#    p.vals = seq(0.1,1,0.1)
#    for(i in 1:length(p.vals)){
#        p = p.vals[i]
#        print(p)
#        indx4 = which(df2[,"p"] == as.character(p))
#        print(indx4)
#        df3 = df2[indx4,]
#        lines(x=df3[,"k"], y=df3[,"score"])
#        text(11, df3[nrow(df3)-1,"score"],  paste("p=",p),
#             cex=1.2, pos=3,col="red")
#    }
#    dev.off()



###############################################################################################
# Score of each measure as a function of p, for the Singleton Clusters transformation.
###############################################################################################

	plot.folder = VISUAL.INSPECTION.FOLDER
	#t = TRANSF.1NEWCLU,
	#t = TRANSF.NEIG.CLU.SWAP
	t = TRANSF.SINGLCLU
	measures = c(SIM.MEASURES["Fm"],SIM.MEASURES["FMI"],SIM.MEASURES["JACCARD"],SIM.MEASURES["HA.ARI"],SIM.MEASURES["RI"],SIM.MEASURES["NMIsum"])

    pdf(file=file.path(plot.folder,paste0("evol-p_AllMeasures_",t,".pdf")))
    plot(NULL, xlim=c(0.1,1), ylim=c(0,1), xlab="p", ylab="measure score", main=paste0("Evolution of measure score by p\nn=6480, h=0, k=5\nt=",t), cex.main=1.5, cex.axis=1.2, cex.lab=1.2)

    for(m in 1:length(measures)){
        measure = measures[m]
        df<-read.csv(file.path(DATA.FRAME.FOLDER, paste0(measure,"-data-frame.csv")), sep=";", stringsAsFactors=F, check.names=F, header=T)

        indx1 = (df[,"t"] == t)
        # indx2 = (df[,"h"] == 0)
        # indx3 = (df[,"n"] == 6480)
        # indx4 = (df[,"k"] == 5)
		indx2 = (df[,"h"] == H.VALS[order(H.VALS)][1]) # the first value among the considered ones
		indx3 = (df[,"n"] == N.VALS[order(N.VALS)][length(N.VALS)/2]) # we pick a medium value
		indx4 = (df[,"k"] == K.VALS[order(K.VALS)][length(K.VALS)/2]) # we pick a medium value
		
		
        indx = which(indx1 & indx2 & indx3 & indx4)
        df2 = df[indx,]     

        lines(x=df2[,"p"], y=df2[,"score"])
        posy = nrow(df2)/2
        posx = SUBSET.PROPS[order(SUBSET.PROPS)][length(SUBSET.PROPS)/2] # a medium value, e.g. 0.5 for [0,1]
#        if(measure == SIM.MEASURES["NMIsum"]){
#            posy = nrow(df2)
#            posx = 1
#        }
        text(posx, df2[posy,"score"],  measure, cex=1.4, pos=3,col="red")
    }
    dev.off()

	

###############################################################################################
# Score of each measure as a function of k, for the Singleton Clusters transformation
###############################################################################################

	plot.folder = VISUAL.INSPECTION.FOLDER
    #t = TRANSF.1NEWCLU,
    #t = TRANSF.NEIG.CLU.SWAP,
    t = TRANSF.SINGLCLU
    measures = c(SIM.MEASURES["Fm"],SIM.MEASURES["FMI"],SIM.MEASURES["JACCARD"],SIM.MEASURES["HA.ARI"],SIM.MEASURES["RI"],SIM.MEASURES["NMIsum"])

    pdf(file=file.path(plot.folder,paste0("evol-k_AllMeasures_",t,".pdf")))
    plot(NULL, xlim=c(min(K.VALS),max(K.VALS)), ylim=c(0,1), xlab="k", ylab="measure score", main=paste0("Evolution of measure score by k\nn=6480, h=0, p=0.5\nt=",t), cex.main=1.5, cex.axis=1.2, cex.lab=1.2)

    for(m in 1:length(measures)){
        measure = measures[m]
        df<-read.csv(file.path(DATA.FRAME.FOLDER, paste0(measure,"-data-frame.csv")), sep=";", stringsAsFactors=F, check.names=F, header=T)

        indx1 = (df[,"t"] == t)
        # indx2 = (df[,"h"] == 0)
        # indx3 = (df[,"n"] == 6480)
        # indx4 = (df[,"p"] == 0.5)
		indx2 = (df[,"h"] == H.VALS[order(H.VALS)][1]) # the first value among the considered ones
		indx3 = (df[,"n"] == N.VALS[order(N.VALS)][length(N.VALS)/2]) # we pick a medium value
		indx4 = (df[,"p"] == SUBSET.PROPS[order(SUBSET.PROPS)][length(SUBSET.PROPS)/2]) # we pick a medium value
        indx = which(indx1 & indx2 & indx3 & indx4)
        df2 = df[indx,]     

        lines(x=df2[,"k"], y=df2[,"score"])
        posy = nrow(df2)/2
        posx = K.VALS[order(K.VALS)][length(K.VALS)/2] # a medium value
#        if(measure == SIM.MEASURES["NMIsum"]){
#            posy = nrow(df2)
#            posx = 1
#        }
        text(posx, df2[posy,"score"],  measure, cex=1.4, pos=3,col="red")
    }
    dev.off()

