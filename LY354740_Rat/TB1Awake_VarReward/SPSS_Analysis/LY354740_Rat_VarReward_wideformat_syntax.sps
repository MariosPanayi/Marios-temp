* Encoding: UTF-8.

DATASET ACTIVATE DataSet4.
GLM AUC_SAL_1 AUC_SAL_2 AUC_SAL_3 AUC_LY_1 AUC_LY_2 AUC_LY_3 Peak_SAL_1 Peak_SAL_2 Peak_SAL_3 
    Peak_LY_1 Peak_LY_2 Peak_LY_3 Latency2peak_SAL_1 Latency2peak_SAL_2 Latency2peak_SAL_3 
    Latency2peak_LY_1 Latency2peak_LY_2 Latency2peak_LY_3
  /WSFACTOR=Drug 2 Polynomial rewardMagnitude 3 Polynomial 
  /MEASURE=AUC Peak Latency2peak 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /EMMEANS=TABLES(Drug*rewardMagnitude) COMPARE(Drug)
    /EMMEANS=TABLES(rewardMagnitude) COMPARE(rewardMagnitude)
  /WSDESIGN=Drug rewardMagnitude Drug*rewardMagnitude.
