



###############################################################################################
# It collects all measure score results into csv files. It generates a csv file per evaluation measure.
#
# n.vals: a vector of values regarding number of nodes
# k.vals: a vector of values regarding number of clusters
# h.vals: a vector of values regarding cluster size heterogeneity
# transf.types: a vector of values regarding transformation types
# subset.props: a vector of values regarding transformation intensity
# comp.measures: a vector of values regarding external evaluation measures
#
###############################################################################################
create.all.measure.data.frame = function(n.vals, k.vals, h.vals, transf.types, subset.props, comp.measures)
{

    data.frame.folder = DATA.FRAME.FOLDER
    tlog(4, "data frame folder is: ", data.frame.folder)
    if(!dir.exists(data.frame.folder))
        dir.create(data.frame.folder, recursive=TRUE)


    for(measure in comp.measures){
        measure.data.frame = c()

        for(transf.type in transf.types){

            for(n in n.vals){
	            for(k in k.vals){	
		            for(h in h.vals){
                        scores = c()
			            for(subset.prop in subset.props){			
                            tlog(4, "measure=",measure,", n=",n, ", k=",k, ", h=",h, " transf.type=",transf.type, ", subset.prop=",subset.prop)
                            eval.folder = get.eval.folder.path(n, k, h, transf.type, subset.prop)
                            eval.fpath = file.path(eval.folder, paste0(measure,".csv"))
                            
                            val=NA
                            if(file.exists(eval.fpath)){
                                # common input for each type of transformation
                                val = read.csv(eval.fpath, header=F)$V1
                            }
                            scores = rbind(scores, val)
			            }

                        # even though subset.props can be adjusted depending on transf.type, use the initial same subset.props
                        # because, for a symmetric transformation, p=0.5 is actually its maximum, so it is like p=1 ==> this is the idea
                        curr.data.frame = data.frame(t=transf.type, n=n, k=k, h=sprintf("%.4f",h), p=sprintf("%.4f",subset.props), score=scores)
                        measure.data.frame  = rbind(measure.data.frame, curr.data.frame)
		            }
	            }
            }

        }

        write.table(x=measure.data.frame, file=file.path(data.frame.folder, paste0(measure,"-data-frame.csv")), row.names=FALSE, col.names=TRUE,sep=";")
    }
	
}
