%% Extract Raw Data
filepath = "C:\Users\Marios\Documents\GitHub\Marios-temp\MP012\RawData\";
filename = "MP_LPCD_1Lever_Degraded_Day1";
data_raw = mpc_read_multiple_data(strcat(filepath,filename));

%% List of relevant Event IDs array A = Event, array B = Time
% \1 = TrialChange
% \5 = LeftLeverResponse
% \7 = RightLeverResponse
% \9 = LeftNP
% \11 = RightNP
% \13 = leftRewardOn
% \15 = rightRewardOn
% \17 = ITI start
% \20 = NonContingent Cue
% M(1) = ^LeftReward,
% M(2) = ^RightReward,
% M(5) = ^DegradedLever,
% M(6) = ^DegradedRewardID

for subj = 1:size(data_raw,2)
    %General Parameters
    data(subj).program = data_raw{1,subj}.MSN;
    data(subj).mouse = data_raw{1,subj}.Subject;
    %Rewarded Lever
    data(subj).leftReward = data_raw{1,subj}.M(1+1);
    data(subj).rightReward = data_raw{1,subj}.M(2+1);
    data(subj).noncontingentReward = data_raw{1,subj}.M(6+1);
    
    
    %N.B. only works for 1 lever design and not in extinction
    if data(subj).leftReward > 0
        data(subj).rewardedLever = 1;
        data(subj).contingentReward = data(subj).leftReward;
        data(subj).contRewardID = 13;
        %event flags for rewarded and nonrewarded LPs
        data(subj).LPr = 5;
        data(subj).LPnr = 7;
    elseif data(subj).rightReward > 0
        data(subj).rewardedLever = 2;
        data(subj).contingentReward = data(subj).rightReward;
        data(subj).contRewardID = 15;
        %event flags for rewarded and nonrewarded LPs
        data(subj).LPr = 7;
        data(subj).LPnr = 5;
    end
    
    if data(subj).contingentReward == data(subj).noncontingentReward
        data(subj).condition = "Degraded";
    else
        data(subj).condition = "NonDegraded";
    end
    
    data(subj).event = data_raw{1,subj}.A;
    data(subj).ts = data_raw{1,subj}.B;
end


%%

for subj = 1:size(data,2)
    data(subj).LPr_total = sum(data(subj).event == data(subj).LPr);
    data(subj).LPnr_total = sum(data(subj).event == data(subj).LPnr);
    data(subj).ILPrI = diff(data(subj).ts((data(subj).event == data(subj).LPr)));
    
end

