


###############################################################################################
# It applies external evaluation measures of interest for given values of n, k, h, transf.type and subset.prop.
#	It stores the result into a file inside a specific folder. The file name is the name of the considered measure.
#	An example path of such folder: "out/evaluations/n=3240_k=2_h=0.2000_t=Singleton Clusters_p=0.7000"
#
# n: number of nodes
# k: number of clusters
# h: cluster size heterogeneity
# transf.type: transformation type
# subset.prop: transformation intesity
# comp.measures: external evaluation measures of interest
# force: whether the evaluation is performed by force, even the corresponding file exists 
#
###############################################################################################
evaluate.partitions = function(n, k, h, transf.type, subset.prop, comp.measures, force=FALSE)
{
	tlog(4, "n=",n, ", k=",k, ", h=",h, " transf.type=",transf.type, ", subset.prop=",subset.prop)

	part.folder = get.part.folder.path(n, k, h, transf.type, subset.prop)
	eval.folder = get.eval.folder.path(n, k, h, transf.type, subset.prop)
	tlog(4, "eval folder is: ", eval.folder)
	
	if(!dir.exists(eval.folder))
		dir.create(eval.folder, recursive=TRUE)
	
    # ===================
    subset.prop = adjust.subset.props(subset.prop, transf.type, k, h) # depending on transformation, we my need to scae the subset.props, i.e. 'k new cluster' is symmetric
    # ===================

	df = NA
	eval.fpath = file.path(eval.folder, "all-results.csv")
	# common input for each type of transformation
	in.mem = generate.membership.vector(n=n, k=k, h=h)
    out.mem = generate.partition(n, k, h, transf.type, subset.prop)
    # =========================
    if(all(is.na(out.mem))) # this is the case mostly when k=2 for "Cluster Neighbor Swaps"
        return(NA)
    # =========================
	
	for(measure in comp.measures){
        eval2.fpath = file.path(eval.folder, paste0(measure,".csv"))
        tlog(4, "eval filepath is: ", eval2.fpath)

        # -----------------------
        # special case: FM yields NaN in this special case: To handle this, 
        #   instead of having all single cluster nodes in 'out.mem', we regroup the last 2 nodes into the same cluster, then the others are single cluster node
        out.mem2 = out.mem
        if(measure == FMI && transf.type == TRANSF.SINGLCLU && subset.prop == 1)
            out.mem2[length(out.mem2)] = length(out.mem2)-1
        # -----------------------

        # inside the function 'compare.partition.pair', we check if the result already exists
        if(!file.exists(eval2.fpath) || force){
			result = compare.partition.pair(eval.folder, in.mem, out.mem2, measure, output.type="distance", log.unit="log", force=force)[measure]
            write.table(x=result, file=eval2.fpath, row.names=FALSE, col.names=FALSE)
        } 
	}

    return(df)
}





###############################################################################################
# It is the starting method in the aim of evaluating partitions through comp.measures.
#   It handles all values of n.vals, k.vals, h.vals, transf.types and subset.props.
#
# n.vals: a vector of values regarding number of nodes
# k.vals: a vector of values regarding number of clusters
# h.vals: a vector of values regarding cluster size heterogeneity
# transf.types: a vector of values regarding transformation types
# subset.props: a vector of values regarding transformation intensity
# comp.measures: a vector of values regarding external evaluation measures
# force: whether or not the existing files are overwritten by a fresh call of all corresponding methods
#
###############################################################################################
evaluate.all.partitions = function(n.vals, k.vals, h.vals, transf.types, subset.props, comp.measures, force=FALSE)
{
    tlog("evluating partitions")
	for(transf.type in transf.types){
	
		for(k in k.vals){
	    	for(h in h.vals){
				for(n in n.vals){
					for(subset.prop in subset.props){
						evaluate.partitions(n, k, h, transf.type, subset.prop, comp.measures, force)
						
					}	
				}
			}
		}	
	}

}
