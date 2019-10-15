%% Extract Raw Data 
filepath = "C:\Users\Marios\Documents\GitHub\Marios-temp\MP012\RawData\";
filename = "MP_LPCD_1Lever_Degraded_Day1";
data = mpc_read_multiple_data(strcat(filepath,filename));

%% List of Event IDs array A = Event, array B = Time
% \1 = TrialChange
% \2 = RewardDelivery
% \3 = MagazineEntry
% \4 = MagazineExit
% \5 = LeftLeverResponse
% \6 = LeftLeverResponseEnd
% \7 = RightLeverResponse
% \8 = RightLeverResponseEnd
% \9 = LeftNP
% \10 = LeftNPEnd
% \11 = RightNP
% \12 = RightNPEnd
% \13 = CS1On
% \14 = CS1Off
% \15 = CS2On
% \16 = CS2Off
% \17 = ITI start
% \18 = NonContingent Cue

subj = 1;
data{1,subj}.A
data{1,subj}.B



