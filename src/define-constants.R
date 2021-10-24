#############################################################################################
# This script allows defining all the file constants used by the other scripts and functions.
# This include both file names and folder names.

#############################################################################################

#############################################################################################
# Folders
#############################################################################################
# main folder
MAIN.FOLDER <- "."

## external libraries folder
#LIB.FOLDER <- file.path(MAIN.FOLDER,"lib")

# general input folder
IN.FOLDER <- file.path(MAIN.FOLDER,"in")

# general ouput folder	
OUT.FOLDER <- file.path(MAIN.FOLDER,"out")
	 # partitions folder
	PART.FOLDER <- file.path(OUT.FOLDER,"partitions")
	# evaluation folder (sim scores)
	EVAL.FOLDER <- file.path(OUT.FOLDER,"evaluations")

	# data frame folder: data frames by measure
	DATA.FRAME.FOLDER <- file.path(OUT.FOLDER,"data-frames")
	
	REGRESSION.RES.FOLDER <- file.path(OUT.FOLDER,"regression-results")
		REGRESSION.COMP.RES.FOLDER = file.path(REGRESSION.RES.FOLDER,"comparison-results")
		REGRESSION.RAW.RES.FOLDER = file.path(REGRESSION.RES.FOLDER,"raw-results")
		
	PLOTS.FOLDER <- file.path(OUT.FOLDER,"plots")
		VISUAL.INSPECTION.FOLDER = file.path(PLOTS.FOLDER, "visual-inspection")
		SIGNIFICANCE.ANALYSIS.FOLDER = file.path(PLOTS.FOLDER, "significance-analysis")
		REL.IMP.ANALYSIS.FOLDER = file.path(PLOTS.FOLDER, "relative-importance-analysis")
	
	
#############################################################################################
# Transformation Types
###########################################################################

TRANSF.SINGLCLU = "Singleton Clusters"
TRANSF.1NEWCLU = "1 New Cluster"
TRANSF.kNEWCLU = "k New Clusters"
TRANSF.NEIG.CLU.SWAP = "Neighbor Cluster Swap"
TRANSF.ALL.CLU.SWAP = "All Cluster Swap"
TRANSF.ORT.CLU = "Orthogonal Clusters"


ABBR.TRANSF.TYPES = list()
    ABBR.TRANSF.TYPES[[TRANSF.SINGLCLU]] = 'sc'
    ABBR.TRANSF.TYPES[[TRANSF.1NEWCLU]] = 'onc'
    ABBR.TRANSF.TYPES[[TRANSF.kNEWCLU]] = 'knc'
    ABBR.TRANSF.TYPES[[TRANSF.NEIG.CLU.SWAP]] = 'ncs'
    ABBR.TRANSF.TYPES[[TRANSF.ORT.CLU]] = 'oc'


#############################################################################################
# Measure names
#############################################################################################	

# auxilary
MI = "MI" # Mutual Information
#H = "Entropy" # Entropy
#N11 = "N11"
#N00 = "N00"
#N10 = "N10"
#N01 = "N01"
ERI = "E(R)" # expected value of Rand
#W1 = "W1"
#W2 = "W2"
EMI = "EMI"


# measures	
NMI.sum = "NMIsum" # Normalized Mutual Information  => see Vinh et al., 2010
NMI.sqrt = "NMIsqrt" # Normalized Mutual Information => see Vinh et al., 2010
NMI.joint = "NMIjoint" # Normalized Mutual Information (joint entropy) => see Vinh et al., 2010
NMI.max = "NMImax" # Normalized Mutual Information => see Vinh et al., 2010
NMI.min = "NMImin" # Normalized Mutual Information => see Vinh et al., 2010

AMI.sqrt = "AMIsqrt" # Adjusted version of  Mutual Information => see Vinh et al., 2010
AMI.sum = "AMIsum" # Adjusted version of  Mutual Information => see Vinh et al., 2010
AMI.min = "AMImin" # Adjusted version of  Mutual Information => see Vinh et al., 2010
AMI.max = "AMImax" # Adjusted version of  Mutual Information => see Vinh et al., 2010

RI = "RI" # Rand index => equation (1) in Vinh et al., 2009
ERI = "ERI" #expected value for Rand index => equation (3) in Vinh et al., 2009
RI.max = "RImax" # theoretical max RI value => equation (3) in Vinh et al., 2009
HA.ARI = "HA's ARI" # Hubert and Arabie's adjusted Rand index => equation (3) in Vinh et al., 2009
#MA.ARI = "MA's ARI" # Morey and Agresti’s RI
JACCARD = "Jaccard"
MM = "MM" # Mirkin metric
FMI = "FMI" # Fowlkes and Mallows’s index
Fm = "F-measure" # Harmonic mean purity (F-measure)
VI = "VI" # Variation of Information
rNMI = "rNMI" # reduced Mutual Information (Newman et al, 2019)
NVI = "NVI" # Normalized version of Variation of Information (Vinh et al, 2010)
#NVI.by.n = "NVI by n" # Normalized version of Variation of Information (nomalized by log(n))
#NVI.by.k = "NVI by k" # Normalized version of Variation of Information (nomalized by 2*log(k)) where k is fixed
SJ = "Split-Join"

PSI = "PSI"

#############################################################################################
# File names
#############################################################################################	

INIT = "init"
MBRHSP.FILENAME.PREFIX = "membership"
IN.MBRHSP.FILENAME.PREFIX = "membership-in"
OUT.MBRHSP.FILENAME.PREFIX = "membership-out"
MBRHSP.FILENAME.CLU.FORMAT.PREFIX = paste0(MBRHSP.FILENAME.PREFIX, "-clu-format")

# eval
RESULTS = "results"
ALL.RESULTS = "all-results"


#############################################################################################
# Plot
#############################################################################################	

PLOT.AS.PDF = "pdf"




