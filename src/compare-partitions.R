
#################################################################
# it is used in the AMI formula
# Note that lfactorial(.) = log(factorial(.)) >> for computational purposes, we use in this form
# Also note that exp(log(factorial(.))) = factorial(.) >> for computational purposes, we use in this form
#	see Eq 24a in Vinh et al., 2009 => Information Theoretic Measures for Clusterings Comparisons: Is a correction for chance is necessary ?
#
# n: number of nodes in the partitions
# n.ij.values: a vector of n.ij values, where n.ij: the number of nodes that are common to i.th cluster in the first partition and j.th cluster in the second partition
# a.i: number of clusters in the first partition
# b.j: number of clusters in the second partition
#
##################################################################
compute.inner.EMI = function(n, n.ij.values, a.i, b.j){
	# why not n.ij, instead of n.ij.values? See the inner sum in Eq 24a
    val = sum((n.ij.values / n) * log((n.ij.values * n) / (a.i * b.j)) * exp(lfactorial(a.i) + 
               lfactorial(b.j) + lfactorial(n - a.i) + lfactorial(n - b.j) - lfactorial(n) - lfactorial(n.ij.values)
               - lfactorial(a.i - n.ij.values) - lfactorial(b.j - n.ij.values) - lfactorial(n - a.i - b.j + n.ij.values)))
    return(val)
}



#################################################################
# Processes all the specified measures comparing both partitions.
# Note that, we slightly adjusted the output range of ARI in order that it gives a non-negative value (because it may give negative value). Now it is in [0,1]
# Note also that distance measures are needed to be handled for normalization, at least in this application.
# Moreover, need to have normalized version of sim measures since we will perform (1-value)
# Final note: you need to know if a measure gives a distance or similarity value
#
# eval.folder: the folder where the measure csv files are present. This may be needed when normalized or adjusted measures are calculated
# partition1: first partition.
# partition2: second partition.
# measures: vector of measure names
# output.type: "similarity" or "distance"
# log.unit: a desired log unit, e.g. log2, log10. This parameter is introduced for rNMI.
# force: whether a measure score is computed by force, even it exists
#
# returns: vector of values corresponding to each specified measures.
#################################################################
compare.partition.pair <- function(eval.folder, partition1, partition2, measures="vi", output.type="similarity", log.unit="log", force=FALSE)
{	# init result vector
	result <- rep(NA,length(measures))
	names(result) <- measures

    # ================================================================================
    # we check if the corresponding measure file has already exist
    # if so, load the result.
    # this verification is done inside the function 'compare.partition.pair', because some measures are calculated based on EMI and MI
    if(length(measures)==1){
        eval.fpath = file.path(eval.folder, paste0(measures,".csv"))
        if(file.exists(eval.fpath) && !force){ #  
            tlog(4, "file exists, reading ..", measures)
            result[measures] = read.csv(file=eval.fpath, header=FALSE, check.names=FALSE)$V1
            return(result)
        }
    }
	# ================================================================================


	library(igraph)
	library(clues)
	library(NMF)
	library(entropy)
    library(genieclust) # interesting: https://genieclust.gagolewski.com/genieclust_compare_partitions.html
    library(mclustcomp)

    n = length(partition1)
    k1 = length(unique(partition1)) # denoted as R in the article of (Newman et al, 2019)
    k2 = length(unique(partition2)) # denoted as S in the article of (Newman et al, 2019)

    a = table(partition1)
    b = table(partition2)

	
	if(MI %in% measures){ # Mutual Information, a simiarlity value
		result[MI] <- entropy::mi.plugin(table(partition1,partition2))
	}
	if(VI %in% measures){ # Variation of Information, a distance value
		if(output.type == "similarity")
			result[VI] = NA
		else{
			result[VI] <- igraph::compare(partition1, partition2, method="vi")
			# then, normalize the result to obtain a value between 0 and 1
			# the max value it might take is log(n) where n is the number of items in a partition
			result[VI] = result[VI]
			# note that, if you compare the partitions from different datasets, dont use this normalization
			# if the nb cluster (k) is fixed, you can nomalize differently. Check the Meila's paper.
		}
	}
	if(NVI %in% measures){ # Normalized VI, a distance value
	    # this normalization is proposed by the following paper:
	    #   Vinh et al. "Information Theoretic Measures for Clusterings Comparison: Variants, Properties, Normalization and Correction for Chance". 2010
	    # The author of the VI measure is also discussed this normalization in her book: 
	    #   Meila. "Handbook of Cluster Analysis, Chapter 27: Criteria for Comparing Clusterings". 2015
		result[NVI] <- igraph::compare(partition1, partition2, method="vi")
		# then, normalize the result to obtain a value between 0 and 1
		result[NVI] = 1 - ( result[NVI] / entropy::entropy.plugin(table(partition1,partition2)) )

		if(output.type != "similarity")
			result[NVI] = 1-result[NVI]
		# Note that there are 2 other normalizations proposed by Meila (but, they are not applicable in our context): 
		#1) normalizing VI by log(n), 2) normalizing VI by 2*log(k), where k (the nb cluster)
	}
	if(SJ %in% measures){ # Split-Join, a distance value
        
		if(output.type == "similarity")
			result[SJ] = NA
		else{
			result[SJ] <- igraph::compare(partition1, partition2, method="split.join")
            # normalize it
            result[SJ] = result[SJ]/(2*length(partition1))
        }
	}
	if(PSI %in% measures){ # Pair Sets Index, a similarity value
		# source: Rezaei and Fränti, Set Matching Measures for External Cluster Validity, 2016
	
        result[PSI] = NA
        if(length(unique(partition1)) > length(unique(partition2)))
            result[PSI] = pair_sets_index(partition2, partition1)
        else
		    result[PSI] = pair_sets_index(partition1, partition2)
		if(output.type != "similarity") # if(output.type == "distance")
			result[PSI] <- 1-result[PSI] # dissimilarity ~ distance
	}
	if(RI %in% measures){ # a similarity value
		result[RI] = igraph::compare(partition1, partition2, method="rand")
		if(output.type != "similarity") # if(output.type == "distance")
			result[RI] <- 1-result[RI] # dissimilarity ~ distance
	}
	if(ERI %in% measures){ # Expected rand index (auxiliary score for HA.ARI)
        sum.a <- sum(sapply(a, function(X) {choose(X, 2)}))
        sum.b <- sum(sapply(b, function(X) {choose(X, 2)}))
        expected.index <- (sum.a * sum.b) / choose(n, 2)
        result[ERI] = expected.index

		if(output.type != "similarity") # if(output.type == "distance")
			result[ERI] <- NA # dissimilarity ~ distance
	}
	if(RI.max %in% measures){ # Theoretically Maximum rand index (auxiliary score for HA.ARI)
        sum.a <- sum(sapply(a, function(X) {choose(X, 2)}))
        sum.b <- sum(sapply(b, function(X) {choose(X, 2)}))
        result[RI.max] <- 0.5*(sum.a + sum.b)

		if(output.type != "similarity") # if(output.type == "distance")
		    result[RI.max] <- NA # dissimilarity ~ distance
	}
	if(HA.ARI %in% measures){ # Hubert and Arabie's ARI, a similarity value
		# computing HA.ARI manually
		# ---
        # RI = sum(sapply(table(a,b), function(X) {choose(X, 2)}))
        # ERI = compare.partition.pair(eval.folder, partition1, partition2, measures="ERI", output.type="similarity", log.unit=log.unit, force=force)
        # RI.max = compare.partition.pair(eval.folder, partition1, partition2, measures="RI.max", output.type="similarity", log.unit=log.unit, force=force)
        # result[HA.ARI] <- (RI - ERI) / (RI.max - ERI);
	
		# computing  HA.ARI through a R package
		# ---
		result[HA.ARI] = clues::adjustedRand(partition1, partition2)["HA"]
		if(result[HA.ARI]<0)result[HA.ARI]=0 # in case ARI yields negative value => it is really rare, especially in our application
		if(output.type != "similarity") # if(output.type == "distance")
			result[HA.ARI] = 1-result[HA.ARI] # dissimilarity ~ distance
	}
#	if(MA.ARI %in% measures){ #  Morey and Agresti’s ARI, a similarity value
#		result[MA.ARI] = clues::adjustedRand(partition1, partition2)["MA"]
#		if(result[MA.ARI]<0)result[MA.ARI]=0 # in case ARI yields negativ value => it is really rare, especially in our application
#		if(output.type != "similarity") # if(output.type == "distance")
#			result[MA.ARI] = 1-result[MA.ARI] # dissimilarity ~ distance
#	}
    if(MM %in% measures){ # Mirkin metric, a similarity value

		if(output.type == "similarity")
			result[MM] = NA
		else{
			result[MM] <- unlist(mclustcomp::mclustcomp(partition1, partition2, type="mirkin")["scores"])
            # normalize it
            result[MM] = result[MM]/(n^2)
        }

	}
	if(FMI %in% measures){ # Fowlkes-Mallows Index, a similarity value
        # Notre that there is an adjusted version of FM for correction for chance
		result[FMI] = clues::adjustedRand(partition1, partition2)["FM"]
		if(output.type != "similarity") # if(output.type == "distance")
			result[FMI] = 1-result[FMI] # dissimilarity ~ distance
	}
	if(JACCARD %in% measures){ # Jaccard Index, a similarity value
		result[JACCARD] = clues::adjustedRand(partition1, partition2)["Jaccard"]
		if(output.type != "similarity") # if(output.type == "distance")
			result[JACCARD] = 1-result[JACCARD] # dissimilarity ~ distance
	}
	if(Fm %in% measures){ # F-measure, a similarity value
		res1 = NMF::purity(as.factor(partition1), as.factor(partition2))
		res2 = NMF::purity(as.factor(partition2), as.factor(partition1))
		result[Fm] = (2*res1*res2)/(res1+res2)
		if(output.type != "similarity") # if(output.type == "distance")
			result[Fm] = 1-result[Fm] # dissimilarity ~ distance
	}
	if(NMI.sum %in% measures){ # Normalized Mutual Information, similarity value

        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)

        result[NMI.sum] = (2*MI)/(Ha+Hb)
		if(output.type != "similarity") # if(output.type == "distance")
			result[NMI.sum] <- 1-result[NMI.sum] # dissimilarity ~ distance
	}
	if(NMI.sqrt %in% measures){ # Normalized Mutual Information, similarity value

        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)

        result[NMI.sqrt] = MI/sqrt(Ha*Hb)
		if(output.type != "similarity") # if(output.type == "distance")
			result[NMI.sqrt] <- 1-result[NMI.sqrt] # dissimilarity ~ distance
	}
	if(NMI.joint %in% measures){ # Normalized Mutual Information, similarity value
        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        contigency.table = table(partition1, partition2) # rows represents clusters of partition1, and columns for partition2
        Hab = entropy.plugin(contigency.table)

        result[NMI.joint] = MI/Hab
		if(output.type != "similarity") # if(output.type == "distance")
			result[NMI.joint] <- 1-result[NMI.joint] # dissimilarity ~ distance
	}
	if(NMI.max %in% measures){ # Normalized Mutual Information, similarity value

        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)

        result[NMI.max] = MI/max(Ha,Hb)
		if(output.type != "similarity") # if(output.type == "distance")
			result[NMI.max] <- 1-result[NMI.max] # dissimilarity ~ distance
	}
	if(NMI.min %in% measures){ # Normalized Mutual Information, similarity value
        # (observation) it gives poor score for: compare.partition.pair(c(seq(1,850),rep(851,150)),c(rep(1,180),rep(2,220),rep(3,200),rep(4,400)), measures=NMI.min)
        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)

        result[NMI.min] = MI/min(Ha,Hb)
		if(output.type != "similarity") # if(output.type == "distance")
			result[NMI.min] <- 1-result[NMI.min] # dissimilarity ~ distance
	}
##	if(NMI %in% measures){ # NMI sum from igraph
##		result[NMI] = igraph::compare(partition1, partition2, method="nmi")
##		if(output.type != "similarity") # if(output.type == "distance")
##			result[NMI] <- 1-result[NMI] # dissimilarity ~ distance
##	}
	if(rNMI %in% measures){ # reduced NMI, a similarity score
        # WARNING: correction term is approximated value. So, for small n values, even for 2 identival partitions we dont get 1
        # test: compare.partition.pair(seq(1,100),c(rep(1,18),rep(2,22),rep(3,20),rep(4,40)), measures=rNMI)
        # test: compare.partition.pair(seq(1,20),c(rep(1,8),rep(2,4),rep(3,3),rep(4,5)), measures=rNMI)

        # source: Newman et al. Improved mutual information measure for classification and community detection. 2019

        n = length(partition1)
        k1 = length(unique(partition1)) # denoted as R in the article
        k2 = length(unique(partition2)) # denoted as S in the article
        print(table(partition1))
        print(table(partition2))

        get.cluster.size = function(partition, k){ return(length(which(partition == k))) }
        cluster.sizes1 = sapply(1:k1, function(k) get.cluster.size(partition1, k))
        cluster.sizes2 = sapply(1:k2, function(k) get.cluster.size(partition2, k))

        compute.correction.term = function(cluster.sizes1, cluster.sizes2){
            n = sum(cluster.sizes1) # or sum(cluster.sizes2)
            k1 = length(cluster.sizes1)
            k2 = length(cluster.sizes2)
            w = n/(n+0.5*k1*k2)
            get.x = function(i, w, k1, n) { return( (1-w)/k1 + (w*cluster.sizes1[i])/n ) }
            get.y = function(i, w, k2, n) { return( (1-w)/k2 + (w*cluster.sizes2[i])/n ) }
            mu = (k1+1)/(k1*sum( sapply(1:k2, function(i) get.y(i,w,k2,n)^2) )) - (1/k1)
            nu = (k2+1)/(k2*sum( sapply(1:k1, function(i) get.x(i,w,k1,n)^2) )) - (1/k2)

            # remark: gamma(5+1) = factorial(5) = 120
            term1 = (k1-1)*(k2-1)*my.log.op(n + 0.5*k1*k2, log.unit)
            term2 = 0.5*(k1+nu-2)*sum(sapply(1:k2, function(i) my.log.op(get.y(i,w,k2,n), log.unit) ))
            term3 = 0.5*(k2+mu-2)*sum(sapply(1:k1, function(i) my.log.op(get.x(i,w,k1,n), log.unit) ))
            term4.1 = lgamma(mu*k1) + lgamma(nu*k2) # TODO lgamma is based on log(), not log2() ... which is contradictory with my.log.op()
            term4.2 = (lgamma(nu) + lgamma(k1))*k2
            term4.3 = (lgamma(mu) + lgamma(k2))*k1
            term4 = 0.5*(term4.1 - term4.2 - term4.3)
            correction.term = term1 + term2 + term3 + term4
            return(correction.term)
        }

        # result[rNMI] = entropy::mi.plugin(table(partition1,partition2)) # mutual information
        contigency.table = table(partition1, partition2) # rows represents clusters of partition1, and columns for partition2
        term1 = lfactorial(n) - sum(lfactorial(cluster.sizes2))
        term2 = 0
        
        for(row.indx in 1:nrow(contigency.table)){ # for each cluster of partition1
            a = cluster.sizes1[row.indx] # equals to sum(contigency.table[row.indx,])
            term2 = term2 + lfactorial(a) - sum(lfactorial(contigency.table[row.indx,]))
        }
        result[rNMI] = (term1 - term2)/n

        correction.term = compute.correction.term(cluster.sizes1, cluster.sizes2)
        result[rNMI] = result[rNMI] - (1/n)*correction.term # still unnormalized
        #result[rNMI] = result[rNMI]/(0.5*(entropy::entropy.plugin(table(partition1)) + entropy::entropy.plugin(table(partition2)) ))


        # normalize the measure  
        denomitor.term1 = lfactorial(n) - sum(lfactorial(cluster.sizes1))
        denomitor.term2 = lfactorial(n) - sum(lfactorial(cluster.sizes2))
        correction.term1 = compute.correction.term(cluster.sizes1, cluster.sizes1)
        correction.term2 = compute.correction.term(cluster.sizes2, cluster.sizes2)
        denomitor = denomitor.term1 + denomitor.term2
        result[rNMI] = 2*n*result[rNMI]/denomitor # normalized here

	    if(output.type != "similarity"){ # if(output.type == "distance")
            # TODO: handle more clearly the small negative values
            if(result[rNMI]<0)
                result[rNMI] = 0
	    	result[rNMI] <- 1-result[rNMI] # dissimilarity ~ distance
        }     
    
	}
    if(EMI %in% measures){ # Expected value of MI (auxiliary score for AMI)
		# ==> R solution
        result[EMI] = 0
        for(i in 1:k1){
            for(j in 1:k2){

                #cur.EMI = NA
                #if(dir.exists(get.EMI.lookup.folder.path(n)) && exists("EMI.lookup")){ # if the variable "EMI.lookup" is already created in the main()
                #    cur.EMI = EMI.lookup[[as.character(n)]][as.character(a[i]),as.character(b[j])]
                #} else {
                        #print("else")
                    n.ij.values <- seq(max(a[i] + b[j] - n, 1), min(a[i], b[j]));
                    cur.EMI = compute.inner.EMI(n, n.ij.values, a[i], b[j])
                #}
                result[EMI] = result[EMI] + cur.EMI
    
            }
        }

        #result[EMI] = EMI
		#if(output.type != "similarity") # if(output.type == "distance")
		#	result[EMI] <- NA 
    }
    if(AMI.sum %in% measures){ # Adjusted MI, a similarity score
		# equivalent to AVI
        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        EMI = compare.partition.pair(eval.folder, partition1, partition2, measures="EMI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)

        result[AMI.sum] = (MI - EMI)/(0.5*(Ha+Hb) - EMI)
		if(output.type != "similarity") # if(output.type == "distance")
			result[AMI.sum] <- 1-result[AMI.sum] # dissimilarity ~ distance
    }
    if(AMI.sqrt %in% measures){ # Adjusted MI, a similarity score

        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        EMI = compare.partition.pair(eval.folder, partition1, partition2, measures="EMI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)

        result[AMI.sqrt] = (MI - EMI)/(sqrt(Ha*Hb) - EMI)
		if(output.type != "similarity") # if(output.type == "distance")
			result[AMI.sqrt] <- 1-result[AMI.sqrt] # dissimilarity ~ distance
    }
    if(AMI.min %in% measures){ # Adjusted MI, a similarity score
        # gives bad score for: compare.partition.pair(c(seq(1,850),rep(851,150)),c(rep(1,180),rep(2,220),rep(3,200),rep(4,400)), measures=AMI.min)
        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        EMI = compare.partition.pair(eval.folder, partition1, partition2, measures="EMI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)
        result[AMI.min] = (MI - EMI)/(min(Ha,Hb) - EMI)
		if(output.type != "similarity") # if(output.type == "distance")
			result[AMI.min] <- 1-result[AMI.min] # dissimilarity ~ distance
    }
    if(AMI.max %in% measures){ # Adjusted MI, a similarity score

        MI = compare.partition.pair(eval.folder, partition1, partition2, measures="MI", output.type="similarity", log.unit=log.unit, force=force)
        EMI = compare.partition.pair(eval.folder, partition1, partition2, measures="EMI", output.type="similarity", log.unit=log.unit, force=force)
        Ha = entropy.plugin(a)
        Hb = entropy.plugin(b)

        result[AMI.max] = (MI - EMI)/(max(Ha,Hb) - EMI)
		if(output.type != "similarity") # if(output.type == "distance")
			result[AMI.max] <- 1-result[AMI.max] # dissimilarity ~ distance
    }
##	if(AMI %in% measures){ # ==> Matlab solution
##		# we need to use octave tool in order to get the result
##		# we provide octave with a string which contains 2 vector def + function call
##		mem1.str = paste0("[",paste(partition1, collapse=" "),"]")
##		mem2.str = paste0("[",paste(partition2, collapse=" "),"]")
##		# addpath('../') in order to recgnize the location of AMI.m file
##		# do not put ';' at the end so that we can get the result of the function
##		eval.str = paste0("addpath(\"../src-common\");","mem1=",mem1.str,"; mem2=",mem2.str,";AMI(mem1,mem2)")
##		# parse the string result
##		cmd = paste0("octave --no-gui --silent --eval '", eval.str,"'")
##		octave.answer = system(cmd, wait=TRUE, intern = TRUE)
##		# octave answer is something like that: "ans =   0.5"
##		tmp = gsub("ans =", "", octave.answer)
##		# remove whitespaces
##		str.res = gsub(" ", "", tmp)
##		# TODO: as.numeric() handles output, for instance as.numeric("Inf") outputs Inf
##		result[AMI] = as.numeric(str.res)
##		# if(result[AMI]<0)result[AMI]=0 # in case AMI yields negativ value => it is really rare, especially in our application
##		if(output.type != "similarity") # if(output.type == "distance")
##			result[AMI] <-  1-result[AMI] # dissimilarity ~ distance
##	}
	
	# TODO one can add the processing of other measures here if needed

	return(result)
}


