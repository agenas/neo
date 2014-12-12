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

# Load common functions
source(paste(baseDir, "/commons/tools.r", sep=""))
source(paste(baseDir, "/commons/options.r", sep=""))

createTestFile <- function(filename) {
  fileConn<-file(filename)
  writeLines("# TEST FILE", fileConn)
  close(fileConn)
  fileConn<-file(filename, open="at")
  writeLines('test', fileConn)
  close(fileConn)
}

main<- function() {
  fileslist <- c("descriptor-oracle.yml", "uno.txt", "due.txt", "tre.txt", "descriptor-central.yml", "quattro.txt", "cinque.txt")
  for (i in 1:length(fileslist)) {
    createTestFile(fileslist[i])
  }

  fileslist <- c("descriptor-oracle.yml|descriptor.yml", "uno.txt", "due.txt", "tre.txt")
  writeZipDescriptorFor("test1.zip", fileslist, TRUE)

  fileslist <- c("descriptor-central.yml|descriptor.yml", "quattro.txt", "cinque.txt", "notpresent.csv")
  writeZipDescriptorFor("test2.zip", fileslist)
}

main()
