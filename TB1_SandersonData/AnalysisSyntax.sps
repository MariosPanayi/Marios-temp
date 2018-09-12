* Encoding: UTF-8.

SameDiff*Time 

DATASET ACTIVATE DataSet0.
MIXED Dopamine BY Geno HouseLED SameDiff WITH Time
  /FIXED=Geno HouseLED SameDiff Time Geno*HouseLED Geno*SameDiff Geno*Time HouseLED*SameDiff 
    HouseLED*Time SameDiff*Time Geno*HouseLED*SameDiff Geno*HouseLED*Time Geno*SameDiff*Time 
    HouseLED*SameDiff*Time Geno*HouseLED*SameDiff*Time | SSTYPE(3)
  /METHOD=REML
    /EMMEANS= TABLES(SameDiff) COMPARE(SameDiff) WITH(Time = 0)
  /EMMEANS= TABLES(Geno*HouseLED*SameDiff) COMPARE(SameDiff) WITH(Time = 0)
    /EMMEANS= TABLES(Geno*HouseLED*SameDiff) COMPARE(Geno) WITH(Time = 0)
  /RANDOM=INTERCEPT HouseLED SameDiff Time  HouseLED*SameDiff HouseLED*Time 
     HouseLED*SameDiff*Time | SUBJECT(Subj) COVTYPE(VC).


SORT CASES  BY HouseLED.
SPLIT FILE SEPARATE BY HouseLED.


MIXED Dopamine BY Geno SameDiff WITH Time
  /FIXED=Geno SameDiff Time Geno*Time SameDiff*Time Geno*SameDiff Geno*SameDiff*Time| SSTYPE(3)
  /METHOD=REML
  /EMMEANS= TABLES(SameDiff) COMPARE(SameDiff) WITH(Time = 0)
    /EMMEANS= TABLES(Geno*SameDiff) COMPARE(SameDiff) WITH(Time = 0)
  /RANDOM=INTERCEPT Time SameDiff   | SUBJECT(Subj) COVTYPE(VC).

SameDiff*Time

SPLIT FILE OFF.


COMPUTE Filter=1.
EXECUTE.

IF  (SameDiff = "First") Filter=0.
EXECUTE.

USE ALL.
FILTER BY Filter.
EXECUTE.



FILTER OFF.
USE ALL.
EXECUTE.
