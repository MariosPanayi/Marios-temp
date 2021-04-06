%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Welcome to the world of hallucination-like perception! This document describes the data and analysis software HALIP presented in "Dopamine Mediates Hallucination-Like Perception in Mice" by Schmack et al. 2021. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACKNOWLEDGMENT: The analysis softward includes code created and kindly made
% available in the public domain by Ian Stevenson (beeswarm.m), Alex
% Henderson (cividis.m), Rob Campbell (sigstar.m), Jose Maria
% Garcia-Valdecasas Bernal (colorgradient.m). Thank you!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


For a quick start, run StartHere.m in MATLAB (version 2020a or newer)

Data are saved in MATLAB mat-files:
mouseData.mat contains all the behavioral mouse data (Fig. 1, Fig. 2, Fig. 6)
humanData.mat contains all the behavioral human data (Fig. 3)

Data are organized in tables:
- 'trialTab' contains trialwise data (one line per line).
- 'sessionTab' contains sessionwise data (one line per session).
- 'subjectTab' contains subjectwise data (one line per subject).


Subjectwise variables MICE:
(-> 'subjectId' provides a unique identifier)

- 'subjectId' (string) subject identification code 
- 'dob' (datetime) date of birth
- 'sex' (string) M-male F-female
- 'genotype' (string) 


Subjectwise variables HUMANS:
(-> 'subjectId' provides a unique identifier)

- 'subjectId' (string) subject identification code 
- 'age' (double)
- 'gender' (string) male/female
- 'smoker' (string) yes/no
- 'handedness' (string) right/left
- 'capsScore' (double) self-reported hallucination-proneness. This is the summary score of all subscales of the Cardiff Anomalous Perception Scale.
- 'sclXXX' (double) self-reported psychopathology of various dimensions. These are the raw scores from the Symptom-Checklist-90R.


Sessionwise variables MICE & HUMANS:
(-> 'subjectId' and 'sessionId' provide a unique identifier)

- 'subjectId' (string) subject identification code 
- 'sessionId' (double) session number 
- 'datetime' (datetime) day of data collection
- 'experiment' (string) name of experiment ('behavioral','expectations','ketamine','optogenetics','optogeneticsHaloperidol')
- 'figure' (string) figure panels in which the data is presented in manuscript 


Trialwise variables 
(-> 'subjectId','sessionId' and 'trialId' provide a unique identifier)

MICE & HUMANS
- 'subjectId' (string) subject identification code 
- 'sessionId' (double) session number 
- 'trialId' (double) trial number
- 'evidence' (double) gives the signal-to-noise ratio of the stimulus presented. Please note that values <-30 are used for no-signal trials on which the signal-to-noise ratio is arbitrary because it is not defined 
- 'choice' (zeros/ones) gives the animal's choice. Zero means the animal reported not detecting a signal, one means the animal reported hearing a signal.
- 'confidence' (double) contains the animal's confidence report. In mice, this is the time they invested to wait for a reward (2s to Inf). Note that trials with time investments < 2s are excluded. Also note that time investments are measured on all error trials, but only on correct catch trials (because on the correct non-catch trials the animals time investment was experimentally controlled by the reward delivery). In humans, this is the continuous confidence rating humans gave by positioning a slider. Note that the ratings were rescaled to 0 and 1 in each subject to account for individual difference in the use of the confidence scale.
- 'outcome' (zeros/ones) ones denote that the animal was correct 
- 'mapping' (2x1 zeros/ones)  contains a 1x2 vector for each trial that denotes how choices were rewarded. The first value denotes the reward probability for the choice encoded by 0, the second value denotes the reward probability for the choice enconded by 1. E.g., [1 0] means that on this trial the choice 0 would have always been rewarded and the choice 1 would never have been rewarded. 

MICE only
- 'catchtrial' (zeros/ones) contains ones for trials where the reward was omitted (or would have been omitted in case the animal was correct)
- 'blockBias' (double) denotes trials in an experimental block with an experimentally controlled proportion of signal trials. 0.3 means that 30% of trials wer signals, 0.5 means that 50% of trials wer signals, 0.7 means 70% of trials were signals, NaN means that signal proportion was not part of the experimental manipulation and 50%.
- 'ketamine' (zeros/ones) denotes trials in sessions with vehicle (0) or ketamine (1). Nan means that no ketamine-related drug injection was performed.
- 'optogenetics' (zeros/ones) denotes trials with (1) or without (0) optogenetic stimulation. NaN means that no optogenetic stimulation was performed (e.g. wild-type animals without fiber implants).
- 'haloperidol' (zeros/ones) denotes trials in sessions with haloperidol (1) or vehicle (0). Note that haloperidol was used to rescue the optogenetics effect and therefore always combined with optogenetic stimulation. NaN means that no haloperidol-related drug injection was performed.


Please see figureMouseBehavior.m, figureMouseExpectations.m, figureMouseKetamine.m, figureMouseOptogenetics.m for analysis examples.

