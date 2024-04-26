This folder contains the plate reader data used to plot growth of strains expressing sfGFP under the control of YTK promoters or promoters selected after scRNAseq modelling. 

For the pYTK-sfGFP strains:
Every '.xlsx' file contained the raw data, with the timepoint of each reading indicated in the file name. 
A table of all recorded values can be found in the 'tidied' object in the global environment. 
Within this object the promoter for each strain is indicated below:

896 = pTDH3
897 = pCCW12
898 = pPGK1
899 = pHHF2
900 = pTEF1
901 = pTEF2
902 = pHHF1
903 = pHTB2
904 = pRPL18B
905 = pALD6
906 = pPAB1
907 = pRET2
908 = pRNR1
909 = pSAC6
910 = pRNR2
911 = pPOP6
912 = pRAD27
913 = pPSP2
914 = pREV1


For the model-selected promoter strains:
Every '.xlsx' file contained the raw data, with the timepoint of each reading indicated in the file name. 
A table of all recorded values can be found in the 'tidied' object in the global environment. 
Within this object the promoter for each strain is indicated below:

ks2 = pFOX2
ks3 = pHCS1
ks4 = pHHT1
ks5 = pTDH2
ks6 = pCDC19
ks7 = pRPL28

There were two biological replicates (rep1 and rep2) in the same plate.
The two replicates were labelled '1' or '2' after the sample name and media condition.
Annotations:
ypd1/2 = ypd replicate 1 or 2
ypdx1/2 = ypd 10% glucose replicate 1 or 2
sm1/2 = ynb replicate 1 or 2
yepg1/2 = yepg replicate 1 or 2



A brief description of how the script works (more information is in the comments of each script):
In the platecurver script, the 'plater' package is used to match sample names in the layout.csv file and OD600 readings in the .xlsx files. Then a series of functions are used to plot the blank-corrected values. 

The 'tidied' object contains the non blank-corrected OD600 readings for all of the samples at different time points. It can be generated from the platecurver script, and found in the environment '...env' files. 

The plots are named '...plot'in the environment, and have been produced after fitting a sigmoidal curve to the blank-corrected OD600 values, at the end of the experiment, using the growthcurver package. The growthcurver package also calculates growth metrics like doubling time, time to midlog etc. (https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html). 

media conditions mentioned in the script:
ypdx = YPD 10% glucose media
ypd = YPD media
sm = YNB media
yepg/yepgminus = YEPG media


File names correspond to the following experiments:
311020... - replicate 1, pYTK-sfGFP
091120... - replicate 2, pYTK-sfGFP
161220... - replicate 3, pYTK-sfGFP
140623... - replicates 1 and 2, model-selected promoters
210623... - replicates 3 and 4, model-selected promoters
300623... - replicate 5, model-selected promoters

