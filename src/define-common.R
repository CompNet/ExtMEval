

################################################################################
# It slightly changes the result of the modular math operation, because 
#	when we type '6 %% 6', we get '0' in this operation.
#	 However, we need to get '1' instead of '0' for convenience.
# So, some example results output by this methods:
# 	my.modular.math.op(6,5) gives '1'
# 	my.modular.math.op(6,6) gives '6'
################################################################################
my.modular.math.op = function(number, divisor){
	num = number %% divisor
	if(num == 0) num = divisor
	return(num)
}



################################################################################
# my log function
# It is used in the computation of rNMI in src/compare-partitions.R
#
################################################################################
my.log.op = function(value, unit="log"){
    
    zero.indx = NA
    if(!is.vector(value) && value == 0)
        return(0)
    else if(is.vector(value))
        zero.indx = which(value == 0)

	if(unit == "log2")
	    result = log2(value)
    else if(unit == "log")
        result = log(value)
     else if(unit == "log10")
        result = log10(value)
     else
        result = log(value)

    if(is.vector(value))
        result[zero.indx] = 0
    return(result)
}





##############################################################################################
##
##############################################################################################
#write.partition.in.cluster.format = function(mbrshp, folder, filename){
#	k = length(unique(mbrshp))
#	content = ""
#	for(i in 1:k){
#		indx = which(mbrshp == i)
#		content = paste0(content, "[", paste(indx, collapse=", "), "]")
#		
#		if(i != k)
#			content = paste0(content, "\n")
#	}
#	
#	write(x=content, file=file.path(folder, filename))
#	
#}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possible some
# offset (to represent the hierarchy of function calls).
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog <- function(offset=NA, ...)
{	prefix <- paste("[",format(Sys.time(),"%a %d %b %Y %X"),"] ",sep="")
	if(!is.na(offset))
	{	if(is.numeric(offset))
		{	os <- paste(rep(".",offset), sep="", collapse="")
			prefix <- paste(prefix, os, sep="")
		}
		else
			prefix <- paste(prefix, offset, sep="")
	}
	cat(prefix, ..., "\n", sep="")
}


#############################################################################################
# It adjusts the values of transformation intensity in case of symmetric transformations. 
#	These transformations are: TRANSF.kNEWCLU and TRANSF.NEIG.CLU.SWAP
#	This method assumes that some of the considered values of transformation intensity are greater than 0.5 and the maximum value is 1.
#	For instance: SUBSET.PROPS = seq(0.1,1.0,0.10)
# 	In these cases, we consider 0.5 as max value reather than 1. In the end, we have: seq(0.05,0.5,0.05)
#	Hence, wheter a given transformation type is symmetric or not, the number of values for transformation intensity is 10.
#
#
# subset.props: a vector of values regarding transformation type. We suppose that some of the considered values are greater than 0.5 and the maximum value is 1.
#				Example: subset.props = seq(0.1,1.0,0.10)
# transf.type: transformation type
# k: number of clusters
# h: cluster size heterogeneity
#
#############################################################################################
adjust.subset.props = function(subset.props, transf.type, k, h){
    # those transformation types are symmetric, so p=1 makes no sense. => divide by 2 
    #if(transf.type == TRANSF.kNEWCLU || (k==2 && transf.type==TRANSF.NEIG.CLU.SWAP) || (h==0 && transf.type==TRANSF.NEIG.CLU.SWAP) ) { # || transf.type == TRANSF.ALL.CLU.SWAP
    if(transf.type == TRANSF.kNEWCLU || transf.type == TRANSF.NEIG.CLU.SWAP) { # || transf.type == TRANSF.ALL.CLU.SWAP
        subset.props = subset.props/2
    }
    return(subset.props)
}

