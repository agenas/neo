####################################################################
# Copyright 2014 Fabrizio Carinci, AGENAS.
#
# Licensed under the European Union Public Licence (EUPL), Version 1.1 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#       http://joinup.ec.europa.eu/software/page/eupl/licence-eupl
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
####################################################################

####################################################################
# Author: Fabrizio Carinci, AGENAS, carinci@agenas.it
# February 2014
####################################################################

# =====================================================
# Entry point
#
# baseDir = Root directory for all indicators
# workDir = Work directory for this indicator
#
# =====================================================

# Check for installed packages
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

# Load common functions
source(paste(baseDir, "/commons/tools.r", sep=""))
source(paste(baseDir, "/commons/options.r", sep=""))

liste_opzioni(installed=0)

# Load Required Libraries

install_library<-function(lib)  {

 update.packages(ask=FALSE,checkBuilt=TRUE,repos="http://cran.r-project.org",oldPkgs=PkgsNeed)

 for (i in 1:length(lib)) {

  if(!is.installed(lib[i])){
   install.packages(lib[i], repos="http://cran.r-project.org")
  }

  suppressMessages(library(lib[i],character.only=TRUE))

 }

 # x<-tryCatch(library(lib[i],character.only=TRUE), error = function(e) e)
 # if(length(grep("was built before",as.character(x[1])))>0) {
 #  install.packages(lib[i], repos="http://cran.r-project.org",dependencies=TRUE)
 # }
 # installed.packages()[,"Built"]
 # available.packages(contriburl=contrib.url('http://cran.r-project.org'))["abc",'Depends']
 # paste(R.version$major,".",R.version$minor,sep="")  library(lib[i],character.only=TRUE)

}

install_library(PkgsNeed)

# Make sure plyr's version is >= 1.8
# pkgs <- installed.packages()
# if (!(pkgs['plyr', 'Version'] >= "1.8")) {
#  remove.packages("plyr")
#  install.packages("plyr", repos="http://cran.r-project.org")