* Encoding: UTF-8.
* Model 1 converges - same as treating each Electrode as indpendent
* Added channel in as a random effect but model did not converge with any combiantion of random factros and channel or by itself. 
* Note that these analyses are identical when considering a 3 level model where channel is nested within subjects.
* Residuals normal in all analyses


MIXED AUC BY drug rewardMagnitude Subj channel
    /FIXED=drug rewardMagnitude drug*rewardMagnitude | SSTYPE(3)
    /METHOD=REML
    /EMMEANS = TABLES(drug*rewardMagnitude) COMPARE(drug)
    /EMMEANS = TABLES(rewardMagnitude) COMPARE(rewardMagnitude)
    /RANDOM=INTERCEPT drug rewardMagnitude | SUBJECT(Subj*channel) COVTYPE(VC).
* Removed:
* drug*rewardMagnitude


MIXED Peak BY drug rewardMagnitude Subj channel
    /FIXED=drug rewardMagnitude drug*rewardMagnitude | SSTYPE(3)
    /METHOD=REML
    /EMMEANS = TABLES(drug*rewardMagnitude) COMPARE(drug)
    /EMMEANS = TABLES(rewardMagnitude) COMPARE(rewardMagnitude)
    /RANDOM=INTERCEPT drug   | SUBJECT(Subj*channel) COVTYPE(VC).
* Removed:
* drug*rewardMagnitude
* rewardMagnitude

MIXED latency2peak BY drug rewardMagnitude Subj channel
    /FIXED=drug rewardMagnitude drug*rewardMagnitude | SSTYPE(3)
    /METHOD=REML
    /EMMEANS = TABLES(drug*rewardMagnitude) COMPARE(drug)
    /EMMEANS = TABLES(rewardMagnitude) COMPARE(rewardMagnitude)
    /RANDOM=INTERCEPT drug rewardMagnitude  | SUBJECT(Subj*channel) COVTYPE(VC).
* Removed:
* drug*rewardMagnitude

*Model 2
* By Subject analysis

MIXED AUC BY drug rewardMagnitude Subj channel
    /FIXED=drug rewardMagnitude drug*rewardMagnitude | SSTYPE(3)
    /METHOD=REML
    /EMMEANS = TABLES(drug*rewardMagnitude) COMPARE(drug)
    /EMMEANS = TABLES(rewardMagnitude) COMPARE(rewardMagnitude)
        /SAVE PRED RESID
    /RANDOM= drug rewardMagnitude channel drug*rewardMagnitude drug*channel
    rewardMagnitude*channel  | SUBJECT(Subj) COVTYPE(VC).
* Removed:
    * drug*rewardMagnitude*channel
    * INTERCEPT
    
MIXED Peak BY drug rewardMagnitude Subj channel
    /FIXED=drug rewardMagnitude drug*rewardMagnitude | SSTYPE(3)
    /METHOD=REML
    /EMMEANS = TABLES(drug*rewardMagnitude) COMPARE(drug)
    /EMMEANS = TABLES(rewardMagnitude) COMPARE(rewardMagnitude)
    /RANDOM=  drug  channel drug*rewardMagnitude drug*channel
 | SUBJECT(Subj) COVTYPE(VC).

* Removed:
    * drug*rewardMagnitude*channel
    * rewardMagnitude*channel 
    * rewardMagnitude
    * INTERCEPT
    
MIXED Latency2peak BY drug rewardMagnitude Subj channel
    /FIXED=drug rewardMagnitude drug*rewardMagnitude | SSTYPE(3)
    /METHOD=REML
    /EMMEANS = TABLES(drug*rewardMagnitude) COMPARE(drug)
    /EMMEANS = TABLES(rewardMagnitude) COMPARE(rewardMagnitude)
    /RANDOM=  rewardMagnitude channel drug*rewardMagnitude drug*channel
    rewardMagnitude*channel  | SUBJECT(Subj) COVTYPE(VC).
* Removed:
    * drug*rewardMagnitude*channel
    * drug
    * INTERCEPT

