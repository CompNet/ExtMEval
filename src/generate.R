###############################################################################

# Author: nejat
###############################################################################



###############################################################################################
#
# input:
#[[1]]
#[1] 1
#
#[[2]]
#[1] 4
#
#[[3]]
#[1] 7
#
#[[4]]
#[1] 2 3
#
#[[5]]
#[1] 5 6
#
#[[6]]
#[1] 8 9

# result:
# {1 4 4 2 5 5 3 6 6}
###################################################################################
convert.membership.list.as.vector = function(mem.list){
	mem.vec = c()
	for(i in 1:length(mem.list)){
		mem.vec = c(mem.vec, mem.list[[i]])
	}
	return(mem.vec)
}



###############################################################################################
#
#
###################################################################################
compute.coef.for.min.clu.size = function(k){
	x.coef = k
	return(x.coef)
}


###############################################################################################
#
# 'd' is the delta value
###################################################################################
compute.coef.for.delta = function(k){
	d.coef = ((k-1)*k)/2
	return(d.coef)
}


###############################################################################################
#
# the minimum cluster size was a parameter before. But, now, we decided to make this process
# without parameters. So, the minimum cluster size will be calculated as:
#  n/(x.coef+d.coef). This expression can be floiat. So, round it with floor()
# For instance: n=36, and k=3 => the max delta can is calculated as follows: 3x + 3d = 36 => so, floor(36/6)=6
###################################################################################
compute.max.delta.value = function(n, k){
	x.coef = compute.coef.for.min.clu.size(k)
	d.coef = compute.coef.for.delta(k)
	max.delta = floor( n/(x.coef+d.coef) )
	return(max.delta)
}



###################################################################################
#
# residual vector is created when the minimum cluster size is not integer, i.e. fractional.
# This depends on the given delta value
#
# for isntance, initial vector would be [0,0,0,0]
# for diff.value=1, it becomes [1,0,0,0]
# for diff.value=2, it becomes [1,0,0,1]
# for diff.value=3, it becomes [1,1,0,1]
#
###################################################################################
create.residual.vector = function(k, diff.value){
	
	if(diff.value > k){
		print("error in create.residual.vector()")
		return(-1)
	}
		
	res.v = rep(0,k)
	counter1 = 1
	counter2 = k
	for(i in 1:diff.value){
		if((i%%2) == 1){
			res.v[counter1] = 1
			counter1 = counter1 + 1
		} else {
			res.v[counter2] = 1
			counter2 = counter2 - 1
		}
	}
	
	return(res.v)
}



###############################################################################################
#
# n: total number of nodes
# k: number of clusters
# h: homogeneity (or cluster size balance) level. Normalized value. 
#       0 means homogene (or balanced) and 1 means "heterogene enough" (or unbalanced).
#	By homogene, we mean that all clusters are of the same size.
#	By heterogene, we mean unbalanced cluster sizes.
# 	For now, we use a simple model to generate unbalanced sizes:
# 	We use a 'delta' constant value which is the difference between 2 consecutive cluster sizes.
# 	For instance, when n=36, k=3, and delta=0, the cluster sizes will be (12,12,12).
#	Likewise, when n=36, k=3, and delta=6, the cluster sizes will be (6,12,18).
# 	The relatively trickiest part is to compute the max delta value based on 'n' and 'k'.
#
#
# Some requirements:
# - when h=0, n should be divided by k. For instance, when n=30 and k=4, 
#	it is not possible to generate an homogene membership whose sizes are: (7.5, 7.5, 7.5, 7.5)
#	We can not round it.
# - max.delta should be at least 10, if you want to test several h values like 0.1, 0.2, 0.3, etc.
#
# result:
#[[1]]
#[1] 1 2 3
#
#[[2]]
#[1] 4 5 6
#
#[[3]]
#[1] 7 8 9
###################################################################################
generate.membership.list = function(n, k, h){
#	l = lapply(1:k, function(i) rep(i,clu.sizes[i]))
#	return(l)

	min.delta = 0
	max.delta = compute.max.delta.value(n, k)
	
	x.coef = compute.coef.for.min.clu.size(k)
	d.coef = compute.coef.for.delta(k)
	
	# find corresponding delta value based on normalized value
	delta = round(h*max.delta)
	
	# find the minimum cluster size
	x = (n - delta*d.coef)/x.coef # note that x might be fractional
	
	# generate membership
	base.sizes = rep(x, k)
	clu.sizes = base.sizes + seq(0,k-1)*delta
	is.integer = all( (round(clu.sizes)-clu.sizes) == 0 )
	
    #print("")
	#print("before cluster sizes 1")
	#print(clu.sizes)
	
	if(!is.integer){
		clu.sizes2 = floor(clu.sizes)
		diff.value = n - sum(clu.sizes2)
		res.vec = create.residual.vector(k, diff.value)
		clu.sizes = clu.sizes2 + res.vec
	}

    #SUBSET.PROPS = c(1/4)
    #print("")
	#print("after cluster sizes ")
    #for(p in SUBSET.PROPS){
    #    cat("p: ",p,"\n")
	#    print(clu.sizes*p)
    #}

	
	l = lapply(1:k, function(i) rep(i,clu.sizes[i]))
	return(l)
}



###############################################################################################
#
# result:
# {1 1 1 2 2 2 3 3 3}
###################################################################################
generate.membership.vector = function(n, k, h){
#generate.membership.vector = function(n, k, h, min.clu.size=6){
	mbrshp = unlist(generate.membership.list(n, k, h))
    # mbrshp = unlist(generate.membership.list(n, k, h, min.clu.size))
	return(mbrshp)
}






###############################################################################################
# 1 New Cluster
# It is not necessary that n is perfectly divised by subset.prop. We use floor() to get the integer part
#
# input:
#[[1]]
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#
#[[2]]
# [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#
#[[3]]
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#
#[[4]]
# [1] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#
# result:
#[[1]]
# [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
#
#[[2]]
# [1] 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3
#
#[[3]]
# [1] 1 1 1 1 1 4 4 4 4 4 4 4 4 4 4
#
#[[4]]
# [1] 1 1 1 1 1 5 5 5 5 5 5 5 5 5 5

###################################################################################
make.1.newcluster.transformation = function(k, subset.prop, mem.list){
	
	out.list = list()
	subsets = c()
	for(i in 1:k){
        clust = mem.list[[i]] # cluster i
        nk = length(clust)
        nsub = floor(nk*subset.prop) # nb node affected by sub.prop in cluster i
		
		mem.list[[i]][1:nsub] = 1 # a vector of first nsub nodes

        if(subset.prop < 1)
		    mem.list[[i]][(nsub+1):nk] = i+1
	}
	return(mem.list)
}



###############################################################################################
# K New Clusters
# Note that, singleton cluster transformation and this transformation are the same for n=3
#
# It is not necessary that n is perfectly divised by subset.prop. We use floor() to get the integer part
#
#
# input:
#[[1]]
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#
#[[2]]
# [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#
#[[3]]
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#
#[[4]]
# [1] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#
# result:
#[[1]]
# [1] 1 1 1 1 1 5 5 5 5 5 5 5 5 5 5
#
#[[2]]
# [1] 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6
#
#[[3]]
# [1] 3 3 3 3 3 7 7 7 7 7 7 7 7 7 7
#
#[[4]]
# [1] 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8
#
###################################################################################
make.k.newcluster.transformation = function(k, subset.prop, mem.list){
	
	out.list = list()
	for(i in 1:k){
        clust = mem.list[[i]] # cluster i
        nk = length(clust)
        nsub = floor(nk*subset.prop) # nb node affected by sub.prop in cluster i
		mem.list[[i]][1:nsub] = i
        if(nk != nsub)
		    mem.list[[i]][(nsub+1):nk] = k+i
	}	
	return(mem.list)

}



###############################################################################################
# Neighbor Cluster Swap
# The swap operation is performed between adjacent clusters.
# It is not necessary that n is perfectly divised by subset.prop. We use floor() to get the integer part
#
# input:
#[[1]]
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#
#[[2]]
# [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#
#[[3]]
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#
#[[4]]
# [1] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#
# result:
#[[1]]
# [1] 4 4 4 4 4 1 1 1 1 1 1 1 1 1 1
#
#[[2]]
# [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
#
#[[3]]
# [1] 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3
#
#[[4]]
# [1] 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4


###################################################################################
make.neigh.clu.swap.transformation = function(k, subset.prop, mem.list){
	
	my.modular.math.op = function(number, divisor){
		num = number %% divisor
		if(num == 0) num = divisor
		return(num)
	}
	
	out.list = list()
	for(i in 1:k){
        curr.clust = mem.list[[i]] # cluster i
        j = my.modular.math.op(i-1, k) # previous cluster id
        prev.clust = mem.list[[j]] # cluster i
        ni = length(curr.clust)
        nj = length(prev.clust)
        nsubi = floor(ni*subset.prop) # nb node affected by sub.prop in cluster i
        nsubj = floor(nj*subset.prop)
		# merge the first nsubj nodes of the prev cluster and the rest (related to nsubi) of the curr cluster
        if(nsubi != ni)
		    out.list[[i]] = c(prev.clust[1:nsubj],curr.clust[(nsubi+1):ni])
        else
            out.list[[i]] = c(prev.clust[1:nsubj])
	}
	
	return(out.list)
}



###############################################################################################
# Another version of Neighbor Cluster Swaps, with the exception that the swap operation can be performed between
#	non-adjacent clusters.
# WARNING: Currently, it is not used in the evaluation framework.
#
# It is not necessary that n is perfectly divised by subset.prop. We use floor() to get the integer part
#
# input:
#[[1]]
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#
#[[2]]
# [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#
#[[3]]
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#
#[[4]]
# [1] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#
# result:
# [[1]]
# [1] 3 3 4 4 1 1 1 1 1 1 1 1 1 1 1
# 
# [[2]]
# [1] 4 4 1 1 2 2 2 2 2 2 2 2 2 2 2
# 
# [[3]]
# [1] 1 1 2 2 3 3 3 3 3 3 3 3 3 3 3
# 
# [[4]]
# [1] 2 2 3 3 4 4 4 4 4 4 4 4 4 4 4
#
###################################################################################
make.all.clu.swap.transformation = function(k, subset.prop, mem.list){
	
    if(k == 2)
        return(NA)

	my.modular.math.op = function(number, divisor){
		num = number %% divisor
		if(num == 0) num = divisor
		return(num)
	}
	
	out.list = list()
	for(i in 1:k){
        curr.clust = mem.list[[i]] # cluster i
        j = my.modular.math.op(i-1, k) # previous cluster id
        l = my.modular.math.op(i-2, k) # previous cluster id
        prev.clust = mem.list[[j]] # cluster i
        prev2.clust = mem.list[[l]] # cluster i
        ni = length(curr.clust)
        nj = length(prev.clust)
        nl = length(prev2.clust)
        nsubi = floor(ni*subset.prop) # nb node affected by sub.prop in cluster i
        if(nsubi %% 2 == 1)
            nsubi = nsubi-1
        nsubj = floor(nj*subset.prop)
        if(nsubj %% 2 == 1)
            nsubj = nsubj-1
        nsubl = floor(nl*subset.prop)
        if(nsubl %% 2 == 1)
            nsubl = nsubl-1

        if(nsubi<2 || nsubj<2 || nsubl<2)
            return(NA)

		# merge the first nsubj nodes of the prev cluster and the rest (related to nsubi) of the curr cluster
		out.list[[i]] = c(prev2.clust[(nsubl/2+1):nsubl],prev.clust[1:(nsubj/2)],curr.clust[(nsubi+1):ni])
	}
	
	return(out.list)
}




###############################################################################################
# Singleton Clusters
# It is not necessary that n is perfectly divised by subset.prop. We use floor() to get the integer part
#
# list(vector): list() function puts the vector, as a whole, into the first index of list 
# as.list(vector): as.list() function puts each item into different index of the list
#
# input:
#[[1]]
#[1] 1 1 1 1 1 1 1 1
#
#[[2]]
#[1] 2 2 2 2 2 2 2 2
#
#[[3]]
#[1] 3 3 3 3 3 3 3 3
#
#[[4]]
#[1] 4 4 4 4 4 4 4 4
#
#
# result:
#[[1]]
#[1] 1 2 3 4 17 17 17 17
#
#[[2]]
#[1] 5 6 7 8 18 18 18 18
#
#[[3]]
#[1] 9 10 11 12 19 19 19 19
#
#[[4]]
#[1] 13 14 15 16 20 20 20 20
#
###################################################################################
make.isolation.transformation = function(k, subset.prop, mem.list){
	clu.counter = 0
	for(i in 1:k){
        clust = mem.list[[i]] # cluster i
        nk = length(clust)
        nsub = floor(nk*subset.prop) # nb node affected by sub.prop in cluster i
		mem.list[[i]][1:nsub] = seq(clu.counter+1, clu.counter+nsub)
		#mem.list[[i]][(nsub+1):nk] = k+i
        clu.counter = clu.counter+nsub
	}

    if(subset.prop < 1){
	    for(i in 1:k){
            clust = mem.list[[i]] # cluster i
            nk = length(clust)
            nsub = floor(nk*subset.prop) # nb node affected by sub.prop in cluster i
		    mem.list[[i]][(nsub+1):nk] = clu.counter+i
	    }
    }
	return(mem.list)
}





###############################################################################################
# Orthogonal Clusters
# It is not necessary that n is perfectly divised by subset.prop. We use floor() to get the integer part
#
# list(vector): list() function puts the vector ,as a whole, into the first index of list 
# as.list(vector): as.list() function puts each item into different index of the list
#
# input:
#[[1]]
#[1] 1 1 1 1 1 1 1 1
#
#[[2]]
#[1] 2 2 2 2 2 2 2 2
#
#[[3]]
#[1] 3 3 3 3 3 3 3 3
#
#[[4]]
#[1] 4 4 4 4 4 4 4 4
#
#
# result:
#[[1]]
#[1] 1 2 3 4 5 5 5 5
#
#[[2]]
#[1] 1 2 3 4 6 6 6 6
#
#[[3]]
#[1] 1 2 3 4 7 7 7 7
#
#[[4]]
#[1] 1 2 3 4 8 8 8 8
#
###################################################################################
make.ort.clu.transformation = function(k, subset.prop, mem.list){
    max.nk = max(sapply(mem.list, function(vec) length(vec))) # in case of imbalanced cluster sizes
    max.nsub = floor(max.nk*subset.prop) # this also indicates the number of cluster to be created	
    clu.counter = max.nsub
	for(i in 1:k){
        clust = mem.list[[i]] # cluster i
        nk = length(clust)
        nsub = floor(nk*subset.prop) # nb node affected by sub.prop in cluster i
		mem.list[[i]][1:nsub] = seq(1, nsub)
        if(subset.prop < 1) # if(nk>(nsub+1))
		    mem.list[[i]][(nsub+1):nk] = clu.counter+i
	}
	return(mem.list)
}



# =============================================================================
# =============================================================================



###############################################################################################
# It select the corresponding tranformation method tranformation in order to perform the partition generation/transformation.
# 
# n: number of nodes
# k: number of clusters
# h: cluster size heterogeneity
# transf.type: transformation type
###############################################################################################
generate.partition = function(n, k, h, transf.type, subset.prop){
    in.l = generate.membership.list(n=n, k=k, h=h)
    #print(in.l)
    #print("--")
    out.l = NA

    if(transf.type == TRANSF.SINGLCLU){
		# ===================================================================
		# isolation begin
		out.l = make.isolation.transformation(k, subset.prop, in.l)
	}
	
	# **********************************************************************
	
	
	if(transf.type == TRANSF.1NEWCLU){
		# new cluster begin ===================================================
		out.l = make.1.newcluster.transformation(k, subset.prop, in.l)
	}
	
	
	# **********************************************************************
	
	
	if(transf.type == TRANSF.kNEWCLU){
		# new cluster begin ===================================================
		out.l = make.k.newcluster.transformation(k, subset.prop, in.l)
	}
	
	# **********************************************************************
	
	
	if(transf.type == TRANSF.NEIG.CLU.SWAP){
		# node interchange begin ===================================================
		out.l = make.neigh.clu.swap.transformation(k, subset.prop, in.l)
	}

	# **********************************************************************
	
	
	if(transf.type == TRANSF.ALL.CLU.SWAP){
		# node interchange begin ===================================================
		out.l = make.all.clu.swap.transformation(k, subset.prop, in.l)
	}

	# **********************************************************************

	if(transf.type == TRANSF.ORT.CLU){
		# ===================================================================
		# isolation begin
		out.l = make.ort.clu.transformation(k, subset.prop, in.l)
	}   

    #print(out.l)
    mbrshp = convert.membership.list.as.vector(out.l)
    return(mbrshp)
}


###############################################################################################
# It generates partitions with respect to transformation types for the considered parameter values.
#	Note that the generated partitions are the ones transformed by the initial ones. 
#	These initial partitions are constructed by the input parameters.
#	It stores the generated/transformed partition into a file inside a specific folder. The file name is "membership-out.txt".
#	An example path of such folder: "out/partitions/n=3240_k=3_h=0.8000_t=1 New Cluster_p=0.6000"
#
# n: number of nodes
# k: number of clusters
# h: cluster size heterogeneity
# transf.type: transformation type
# subset.prop: transformation intesity
#
###############################################################################################
generate.partitions = function(n, k, h, transf.type, subset.prop)
{
	tlog(4, "n=",n, ", k=",k, ", h=",h, " transf.type=",transf.type, ", subset.prop=",subset.prop)
	part.folder = get.part.folder.path(n, k, h, transf.type, subset.prop)
	tlog(4, "partition folder is: ", part.folder)
	
	if(!dir.exists(part.folder))
		dir.create(part.folder, recursive=TRUE)
	
	# common input for each type of transformation
	in.l = generate.membership.list(n=n, k=k, h=h)
	
	mbrshp = generate.partition(n, k, h, transf.type, subset.prop)
	# write in table format
	write.table(x=mbrshp, file=file.path(part.folder, paste0(OUT.MBRHSP.FILENAME.PREFIX,".txt")), row.names=FALSE, col.names=FALSE)
	# **********************************************************************
}








###############################################################################################
# It is the starting method in the aim of generating partitions with respect to transformation types.
#   It handles all values of n.vals, k.vals, h.vals, transf.types and subset.props.
#
# n.vals: a vector of values regarding number of nodes
# k.vals: a vector of values regarding number of clusters
# h.vals: a vector of values regarding cluster size heterogeneity
# transf.types: a vector of values regarding transformation types
# subset.props: a vector of values regarding transformation intensity
#
###############################################################################################
generate.all.partitions = function(n.vals, k.vals, h.vals, transf.types, subset.props)
{
	for(transf.type in transf.types){
	    for(n in n.vals){
		    for(k in k.vals){	
			    for(h in h.vals){
					for(subset.prop in subset.props){			
						generate.partitions(n, k, h, transf.type, subset.prop)
					}
				}
			}
		}
	}
	
}
