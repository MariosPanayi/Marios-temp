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

%% Save data to excel file
 writetable(data, 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1DataLongFormat_BaselinesNaNFilts.xlsx');


