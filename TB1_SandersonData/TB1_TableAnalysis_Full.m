%Optional Reset
Reset = 1;
    if Reset
    clear all
    close all
    clc
    end


%% Load data
load('C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1DataLongFormat.mat');

% Simplify naming
data = TB1paramswithextrainfo100nan300300reward4841S1;
clear TB1paramswithextrainfo100nan300300reward4841S1

% Table Colnames
%{'uid', 'Genotype', 'Mouse', 'Session', 'Channel', 'Time_bin', 'Dopamine', 'HouseLED', 'PreviousSameDiff', 'NextSameDiff', 'FirstSecond', 'subject', 'LightNo', 'nans','rewardTTL'} 

%% Calculate baseline

% Extract location of each individual trial per animal/session/channel contained in 'uid'
baseline = data(:, {'uid', 'LightNo'});
[G_baseline,baseline] = findgroups(baseline);

% Calculate baseline period from 0.5s pre cue (scan Num 195) 
for i = min(G_baseline):max(G_baseline)
index = find(G_baseline == i);
baseline2s(index) = nanmean(data.Dopamine(index(175:195)));
baseline2_5s(index) = nanmean(data.Dopamine(index(170:195)));
baseline3s(index) = nanmean(data.Dopamine(index(165:195)));
baseline4s(index) = nanmean(data.Dopamine(index(155:195)));
baseline5s(index) = nanmean(data.Dopamine(index(145:195)));
end

% Create new variables in data table with baseline subtracted 
data.baseSub2 = data.Dopamine - baseline2s';
data.baseSub2_5 = data.Dopamine - baseline2_5s';
data.baseSub3 = data.Dopamine - baseline3s';
data.baseSub4 = data.Dopamine - baseline4s';
data.baseSub5 = data.Dopamine - baseline5s';

%% Calculate NaN percentage at different points
% Extract location of each individual trial per animal/session/channel contained in 'uid'
baseline = data(:, {'uid', 'LightNo'});
[G_baseline,baseline] = findgroups(baseline);

% Calculate percentage NaNs in 5s bins 
for i = min(G_baseline):max(G_baseline)
index = find(G_baseline == i);
NaN_1(index) = sum(isnan(data.Dopamine(index(1:50))))*2;
NaN_2(index) = sum(isnan(data.Dopamine(index(51:100))))*2;
NaN_3(index) = sum(isnan(data.Dopamine(index(101:150))))*2;
NaN_4(index) = sum(isnan(data.Dopamine(index(151:200))))*2;
NaN_5(index) = sum(isnan(data.Dopamine(index(201:250))))*2;
NaN_6(index) = sum(isnan(data.Dopamine(index(251:300))))*2;
NaN_7(index) = sum(isnan(data.Dopamine(index(301:350))))*2;
NaN_8(index) = sum(isnan(data.Dopamine(index(351:400))))*2;
end

% Create new variables in data table with NaN info
data.NaN_1 = NaN_1';
data.NaN_2 = NaN_2';
data.NaN_3 = NaN_3';
data.NaN_4 = NaN_4';
data.NaN_5 = NaN_5';
data.NaN_6 = NaN_6';
data.NaN_7 = NaN_7';
data.NaN_8 = NaN_8';

%% Create Time Bins 

data.Bin1s = ceil(data.Time_bin/1);
data.Bin2s = ceil(data.Time_bin/2);
data.Bin2_5s = ceil(data.Time_bin/2.5);
data.Bin5s = ceil(data.Time_bin/5);
data.Bin10s = ceil(data.Time_bin/10);


%% Calculate Reward Deliery at different points
% Extract location of each individual trial per animal/session/channel contained in 'uid'
baseline = data(:, {'uid', 'LightNo'});
[G_baseline,baseline] = findgroups(baseline);

% Calculate Rewards delivered in 5s bins 
for i = min(G_baseline):max(G_baseline)
index = find(G_baseline == i);
Reward_1(index) = nansum(data.rewardTTL(index(1:50)));
Reward_2(index) = nansum(data.rewardTTL(index(51:100)));
Reward_3(index) = nansum(data.rewardTTL(index(101:150)));
Reward_4(index) = nansum(data.rewardTTL(index(151:200)));
Reward_5(index) = nansum(data.rewardTTL(index(201:250)));
Reward_6(index) = nansum(data.rewardTTL(index(251:300)));
Reward_7(index) = nansum(data.rewardTTL(index(301:350)));
Reward_8(index) = nansum(data.rewardTTL(index(351:400)));
end

% Create new variables in data table with NaN info
data.Reward_1 = Reward_1';
data.Reward_2 = Reward_2';
data.Reward_3 = Reward_3';
data.Reward_4 = Reward_4';
data.Reward_5 = Reward_5';
data.Reward_6 = Reward_6';
data.Reward_7 = Reward_7';
data.Reward_8 = Reward_8';



%% Save data to excel file
%  writetable(data, 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1DataLongFormat_BaselinesNaNFilts.xlsx');

%% Filter Data - NaNs

filt1 = data.NaN_1 > 0;
filt2 = data.NaN_2 > 0;
filt3 = data.NaN_3 > 20;
filt4 = data.NaN_4 > 20;
filt5 = data.NaN_5 > 20;
filt6 = data.NaN_6 > 20;
filt7 = data.NaN_7 > 0;
filt8 = data.NaN_8 > 0;

nanFilter = filt1+ filt2 +filt3 +filt4 +filt5 +filt6 +filt7 +filt8;

nanFilter = ~nanFilter;
%% Filter Data - Rewards
% inequality direction used for filter here

filt1 = data.Reward_1 < 0;
filt2 = data.Reward_2 < 0;
filt3 = data.Reward_3 < 0;
filt4 = data.Reward_4 > 0;
filt5 = data.Reward_5 > 0;
filt6 = data.Reward_6 > 0;
filt7 = data.Reward_7 < 0;
filt8 = data.Reward_8 < 0;

RewardFilter = filt1+ filt2 +filt3 +filt4 +filt5 +filt6 +filt7 +filt8;

RewardFilter = ~RewardFilter;

%% Filter Data - TimePeriod

timeFilt = (0 < data.Bin1s) & (data.Bin1s < 11);

%% Collate data post filtering
%  Table Colnames
% 
% {'uid','Genotype','Mouse','Session','Channel','Time_bin','Dopamine','HouseLED','PreviousSameDiff','NextSameDiff','FirstSecond','subject','LightNo','nans','rewardTTL','baseSub2','baseSub2_5','baseSub3','baseSub4','baseSub5','NaN_1','NaN_2','NaN_3','NaN_4','NaN_5','NaN_6','NaN_7','NaN_8','Bin1s','Bin2s','Bin2_5s','Bin5s','Bin10s','Reward_1','Reward_2','Reward_3','Reward_4','Reward_5','Reward_6','Reward_7','Reward_8'}

fullFilter = all([nanFilter, RewardFilter, timeFilt],2);

filteredData = data(fullFilter,{'uid','Genotype','Mouse','Session','Channel','Time_bin','Dopamine','HouseLED','PreviousSameDiff','NextSameDiff','FirstSecond','subject','LightNo','nans','rewardTTL','baseSub2','baseSub2_5','baseSub3','baseSub4','baseSub5','NaN_1','NaN_2','NaN_3','NaN_4','NaN_5','NaN_6','NaN_7','NaN_8','Bin1s','Bin2s','Bin2_5s','Bin5s','Bin10s','Reward_1','Reward_2','Reward_3','Reward_4','Reward_5','Reward_6','Reward_7','Reward_8'});

% Collapse across trial number and in 1s bins
avgTrials =  filteredData(:,{'uid','subject','Genotype','Mouse','Session','Channel','HouseLED','PreviousSameDiff','NextSameDiff','FirstSecond','Bin5s','Bin2s','Bin1s'});
[G,avgTrials] = findgroups(avgTrials);
avgTrials.Dopamine = splitapply(@nanmean, filteredData.Dopamine, G);
avgTrials.baseSub2 = splitapply(@nanmean, filteredData.baseSub2, G);
avgTrials.baseSub2_5 = splitapply(@nanmean, filteredData.baseSub2_5, G);
avgTrials.baseSub3 = splitapply(@nanmean, filteredData.baseSub3, G);
avgTrials.baseSub4 = splitapply(@nanmean, filteredData.baseSub4, G);
avgTrials.baseSub5 = splitapply(@nanmean, filteredData.baseSub5, G);

% Filter for only 2nd Light trials, Same/Diff Identity
secondLight = strcmp(avgTrials.FirstSecond,'Second light');
sameDiff = ~strcmp(avgTrials.PreviousSameDiff,'First');
SameDifftrialFilter = all([secondLight,sameDiff], 2);

avgTrials_SameDiff = avgTrials(SameDifftrialFilter,:);



%% Save data to excel file
  %writetable(avgTrials, 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1DataLongFormat_Reward5sPre10spost_NaN10pre10post_FullData.xlsx');

writetable(data, 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1DataLongFormat_NoFilters.xlsx');





