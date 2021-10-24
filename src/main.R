
options(digits=17)
source("src/define-imports.R")

SIM.MEASURES = c(
	RI=RI,
    ERI=ERI,
    RImax=RI.max,
	HA.ARI=HA.ARI,
	FMI=FMI,
	JACCARD=JACCARD,
	Fm=Fm,
    MI=MI,
	NMIsum=NMI.sum,
	NMIsqrt=NMI.sqrt,
	NMIjoint=NMI.joint,
	NMImin=NMI.min,
	NMImax=NMI.max,
    EMI=EMI,
    rNMI=rNMI,
	AMIsum=AMI.sum,
	AMIsqrt=AMI.sqrt,
	AMImin=AMI.min,
	AMImax=AMI.max,
    PSI=PSI
	)

DIST.MEASURES = c(
    MM=MM,
	SJ=SJ,
	VI=VI,
	NVI=NVI
	)

COMP.MEASURES = c(
        SIM.MEASURES["RI"],
#        SIM.MEASURES["ERI"]
#        SIM.MEASURES["RImax"]
        SIM.MEASURES["HA.ARI"],
        #DIST.MEASURES["MM"],
        SIM.MEASURES["FMI"],
		SIM.MEASURES["JACCARD"],
        SIM.MEASURES["Fm"],
#        DIST.MEASURES["SJ"],
#        SIM.MEASURES["PSI"],
#        SIM.MEASURES["MI"]
        SIM.MEASURES["NMIsum"]
#        SIM.MEASURES["NMIsqrt"],
#        SIM.MEASURES["NMIjoint"],
#        SIM.MEASURES["NMImin"],
#        SIM.MEASURES["NMImax"],
#        SIM.MEASURES["EMI"]
#        SIM.MEASURES["rNMI"],
#         SIM.MEASURES["AMIsum"],
#         SIM.MEASURES["AMIsqrt"],
#         SIM.MEASURES["AMImin"],
#         SIM.MEASURES["AMImax"]
#        #DIST.MEASURES["VI"]
        #DIST.MEASURES["NVI"]
         )



# Number of elements/nodes in the original partitions
N.VALS = seq(1080,1080*4,1080)


# Number of clusters considered in the original partitions (before applying the transformations)
K.VALS = c(2,4,6,8)


# Cluster size homogeneity
# 0 means 'balanced' and 1 means 'unbalanced enough'.
#	By 'balanced', we mean that all clusters are of the same size.
#	By 'unbalanced', we mean unbalanced cluster sizes.
H.VALS = seq(0,0.90,0.20)


# Transformation types
TRANSF.TYPES = c(
    TRANSF.SINGLCLU, # All the elements affected by this transformation become singletons, i.e. single-element clusters.
    TRANSF.1NEWCLU, # It takes a proportion of each original cluster, and it gathers these elements to create a single cluster.
    TRANSF.kNEWCLU, # It takes a proportion of each original cluster, and it gathers these elements to create k distinct clusters.
    TRANSF.NEIG.CLU.SWAP, # It moves a proportion of each cluster into its neighbor cluster.
						  # Each cluster swaps elements with exactly one different cluster.
    TRANSF.ORT.CLU # This transformation uses a proportion of each cluster to create new clusters, 
				   # in such a way that all of their elements come from different original clusters.
				   # The resulting clusters are orthogonal to the original ones, in the sense that 
				   # each original cluster is represented equally in the new clusters.
)


# Transformation Intensity
# 0 means that there is no transformation applied onto the original partitions 
#	and 0.5 means that the half of the cluster elements in each cluster are used for transformation.
SUBSET.PROPS = seq(0.2,1.0,0.20)




# ====================================
### The considered parameter values in the article
#N.VALS = seq(3240,3240*4,1080)
#K.VALS = seq(2,11)
#H.VALS = seq(0,0.90,0.10)
#SUBSET.PROPS = seq(0.1,1.0,0.10)
# ====================================







################################################################
# Main
#################################################################

## ############### generate.all.partitions
generate.all.partitions(N.VALS, K.VALS, H.VALS, TRANSF.TYPES, SUBSET.PROPS)
 
################ evaluate.all.partitions
evaluate.all.partitions(N.VALS, K.VALS, H.VALS, TRANSF.TYPES, SUBSET.PROPS, COMP.MEASURES, force=FORCE)

################ write into file
create.all.measure.data.frame(N.VALS, K.VALS, H.VALS, TRANSF.TYPES, SUBSET.PROPS, COMP.MEASURES)

################ plot.evaluation.stats
plot.evol.stats(N.VALS, K.VALS, H.VALS, TRANSF.TYPES, SUBSET.PROPS, COMP.MEASURES)

################
source("src/plot-cases-of-interest.R")

###############
compute.all.relaimp(TRANSF.TYPES, COMP.MEASURES) 

###############
plot.all.relaimp(TRANSF.TYPES, COMP.MEASURES)

###############
perform.significance.analysis(TRANSF.TYPES, COMP.MEASURES)
