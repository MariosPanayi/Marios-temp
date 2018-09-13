* Encoding: UTF-8.


DATASET ACTIVATE DataSet0.
MIXED Dopamine BY Geno HouseLED SameDiff WITH Time
  /FIXED=Geno HouseLED SameDiff Time Geno*HouseLED Geno*SameDiff Geno*Time HouseLED*SameDiff 
    HouseLED*Time SameDiff*Time Geno*HouseLED*SameDiff Geno*HouseLED*Time Geno*SameDiff*Time 
    HouseLED*SameDiff*Time Geno*HouseLED*SameDiff*Time | SSTYPE(3)
  /METHOD=REML
     /EMMEANS= TABLES(Geno*HouseLED) COMPARE(HouseLED) WITH(Time = 0)
  /EMMEANS= TABLES(Geno*HouseLED*SameDiff) COMPARE(SameDiff) WITH(Time = 0)
    /EMMEANS= TABLES(Geno*HouseLED*SameDiff) COMPARE(Geno) WITH(Time = 0)
  /RANDOM=INTERCEPT HouseLED SameDiff Time  HouseLED*SameDiff HouseLED*Time SameDiff*Time 
     HouseLED*SameDiff*Time | SUBJECT(Mouse*Channel*Session) COVTYPE(VC).


SORT CASES  BY HouseLED.
SPLIT FILE SEPARATE BY HouseLED.


MIXED Dopamine BY Geno SameDiff WITH Time
  /FIXED=Geno SameDiff Time Geno*Time SameDiff*Time Geno*SameDiff Geno*SameDiff*Time| SSTYPE(3)
  /METHOD=REML
  /EMMEANS= TABLES(SameDiff) COMPARE(SameDiff) WITH(Time = 0)
    /EMMEANS= TABLES(Geno*SameDiff) COMPARE(SameDiff) WITH(Time = 0)
  /RANDOM=INTERCEPT Time SameDiff  SameDiff*Time | SUBJECT(Subj) COVTYPE(VC).

SameDiff*Time

SPLIT FILE OFF.



SORT CASES  BY Geno.
SPLIT FILE SEPARATE BY Geno.


MIXED Dopamine BY HouseLED SameDiff WITH Time
  /FIXED=HouseLED SameDiff Time HouseLED*Time SameDiff*Time HouseLED*SameDiff HouseLED*SameDiff*Time| SSTYPE(3)
  /METHOD=REML
  /EMMEANS= TABLES(SameDiff) COMPARE(SameDiff) WITH(Time = 0)
    /EMMEANS= TABLES(HouseLED*SameDiff) COMPARE(SameDiff) WITH(Time = 0)
  /RANDOM=INTERCEPT HouseLED SameDiff Time HouseLED*Time SameDiff*Time HouseLED*SameDiff HouseLED*SameDiff*Time| SUBJECT(Subj) COVTYPE(VC).





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


MIXED Dopamine BY Geno HouseLED EarlyLate WITH Time
    /FIXED=Geno HouseLED EarlyLate Time Geno*HouseLED Geno*EarlyLate Geno*Time HouseLED*EarlyLate
    HouseLED*Time EarlyLate*Time Geno*HouseLED*EarlyLate Geno*HouseLED*Time Geno*EarlyLate*Time
    HouseLED*EarlyLate*Time Geno*HouseLED*EarlyLate*Time | SSTYPE(3)
    /METHOD=REML
        /EMMEANS= TABLES(Geno*EarlyLate) COMPARE(EarlyLate) WITH(Time = 0)
                /EMMEANS= TABLES(Geno*EarlyLate) COMPARE(EarlyLate) WITH(Time = 1)
                        /EMMEANS= TABLES(Geno*EarlyLate) COMPARE(EarlyLate) WITH(Time = 2)
    /EMMEANS= TABLES(Geno*HouseLED*EarlyLate) COMPARE(EarlyLate) WITH(Time = 0)
    /RANDOM=INTERCEPT Time EarlyLate EarlyLate*HouseLED HouseLED*Time EarlyLate*HouseLED*Time | SUBJECT(Mouse*Channel*Session) COVTYPE(VC).

 HouseLED
EarlyLate*Time 
