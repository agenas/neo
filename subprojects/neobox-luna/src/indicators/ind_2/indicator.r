# =====================================================
# Entry point
#
# baseDir = Root directory for all indicators
# workDir = Work directory for this indicator
#
# =====================================================

# Load common functions
source(paste(baseDir, "/commons/tools.r", sep=""))
source(paste(baseDir, "/commons/options.r", sep=""))

# Load implementation
source(paste(baseDir,"/ind_2/implementation.r", sep=""))

# Load options
liste_opzioni()

# entry point
indicator2()

rm(list=ls(all=TRUE))
