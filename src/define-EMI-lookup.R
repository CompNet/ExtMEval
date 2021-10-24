

#################################################################
# EMI is used in the AMI formula.
# Since the computation of EMI can be repetitive, we reduce the computation time through a lookup matrix
# This functionnality is not used anymore, since the AMI measure is taken into account in the article.
# see the file containing "if(EMI %in% measures){" in src/compare-partitions.R if you want this method to be included in the computations
#
# folder: the folder, where the lookup amtrix is stored
# n: number of nodes in the partitions
# ai.values: all cluster sizes
# max.bj: a aximum possible cluster size for the second partition, it is normally equal to 'n'
##################################################################
write.lookup.EMI.into.file = function(folder, n, ai.values, max.bj){
    library("parallel", warn.conflicts=T, quietly=T);
    use.cores = 6
    # BUG: https://stackoverflow.com/questions/51986674/mclapply-sendmaster-error-only-with-rscript

    emi.lookup <- mclapply(
      ai.values,
      function (a.i) {
        unlist(lapply(
          seq(1, max.bj),
          function (b.j) {
			  n.ij.values <- seq(max(a.i + b.j - n, 1), min(a.i, b.j)); # see Eq 24a in Vinh et al., 2009
           compute.inner.EMI(n, n.ij.values, a.i, b.j)
          }
        ));
      },
      mc.cores = use.cores
    );

    #print("matrix")
    emi.lookup.mtrx = c()
    for(r in emi.lookup){
        emi.lookup.mtrx = rbind(emi.lookup.mtrx, r) # up to 4th digit sprintf("%.4f",r)
    }
    #return(emi.lookup.mtrx)
    #print(length(ai.values))
    #print(nrow(emi.lookup.mtrx))
    #print(ncol(emi.lookup.mtrx))
    rownames(emi.lookup.mtrx) = ai.values
    colnames(emi.lookup.mtrx) = seq(1,n)

    #print("write")
    mtrx.file = file.path(folder, "EMI-lookup-matrix.csv")
    write.csv(x=emi.lookup.mtrx, file=mtrx.file)
    # as.matrix(read.csv(mtrx.file, row.names = 1, header= TRUE, check.names=FALSE))
}


#################################################################
# For a fixed 'n', it retreives all distinct cluster sizes for
#   all the considered 'k' and 'h' values.
#
# n: number of nodes
# k.vals: a vector of the considered values for number of clusters
# h.vals: a vector of the considered values for cluster size heterogeneity
#
##################################################################
retreive.cluster.sizes = function(n, k.vals, h.vals){
    all.vals = c()
    for(k in k.vals){	
        for(h in h.vals){
		    vals = table(generate.membership.vector(n, k, h))
            all.vals = c(all.vals, vals)
	    }
    }
    u.vals = unique(all.vals)
    return(u.vals[order(u.vals)]) # sorted unique values
}


#################################################################
# It creates a lookup matrix of EMI values for each considered n value.
#
# n.vals: a vector of the considered values for number of nodes
# k.vals: a vector of the considered values for number of clusters
# h.vals: a vector of the considered values for cluster size heterogeneity
#
##################################################################
retreive.all.cluster.sizes = function(n.vals, k.vals, h.vals){

    for(n in n.vals){
        #print(n)
		all.cluster.sizes = retreive.cluster.sizes(n, k.vals, h.vals)
        #print(all.cluster.sizes)
        folder = get.EMI.lookup.folder.path(n)
        if(!dir.exists(folder))
		    dir.create(folder, recursive=TRUE)
            
        write.lookup.EMI.into.file(folder, n, all.cluster.sizes, n)
	}
    
}

