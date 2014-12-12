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


  ###################################################################
 # Stress tests for NEO
 # Author: Fabrizio Carinci, January 2014
 ###################################################################
 # This file tests the robustness of the system under the following
 # conditions for Input and Reference data:
 # Input Data: no data, no diseases, ok
 # Ref Data: no data, no diseases, ok
 # There are N=9 combinations, one is excluded (normal condition, Input ok; Ref ok
 # You can uncomment one by one and test all indicators simultaneously
 ###################################################################
 # Note: tested on Brescia sample data (N=400,000) with select_unit=DIST_MMG=='004'
 ###################################################################

 # input_data<-subset(input_data,COUNT<0)          # Input no data; Ref ok
 # input_data<-subset(ref_data,COUNT>100)          # Input no diseases; Ref ok
 # ref_data<-subset(ref_data,COUNT<0)              # Input ok; Ref no data
 # ref_data<-subset(ref_data,COUNT>100)            # Input ok; Ref no diseases
 # input_data<-subset(ref_data,COUNT>100);ref_data<-subset(ref_data,COUNT>100) # Input no diseases; Ref no diseases
 # input_data<-subset(ref_data,COUNT>100);ref_data<-subset(ref_data,COUNT<0)   # Input no diseases; Ref no data
 # input_data<-subset(input_data,COUNT<0);ref_data<-subset(ref_data,COUNT<0)   # Input no data; Ref no data
 # input_data<-subset(input_data,COUNT<0);ref_data<-subset(ref_data,COUNT>100) # Input no data; Ref no diseases
