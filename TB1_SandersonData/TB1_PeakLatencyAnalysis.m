%Optional Reset
Reset = 1;
    if Reset
    clear all
    close all
    clc
    end
%%
%% Load data
data = readtable('C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1_AllLights_DataforPeakAnalysis.xlsx');

%%
% Find groups
% 'Time_bin', 'Dopamine'

index_3s  = find(data.Time_bin > 0 & data.Time_bin < 3);
data_3s = data(index_3s, :);

peaks = data_3s(:, {'Genotype', 'Mouse', 'Session', 'Channel', 'subject','HouseLED','EarlyLate', 'LightNo'});
[G_peaks, peaks] = findgroups(peaks);

%Create smoothing filter and apply
smooth3 = @(x) {smooth(x,3)};
smoothed = splitapply(smooth3, data_3s.Dopamine, G_peaks);
smoothed = vertcat(smoothed{:});
data_3s.smooth3 = smoothed;


%find peaks
[peaks.peakVal, peaks.peakTime] = splitapply(@max, data_3s.Dopamine, G_peaks);
[peaks.smoothPeakVal, peaks.smoothPeakTime] = splitapply(@max, data_3s.smooth3, G_peaks);


writetable(peaks, 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1_Lights_PeaksAnalysis.xlsx');

%% Repeat for Rewards Data
%Optional Reset
Reset = 1;
    if Reset
    clear all
    close all
    clc
    end
    %


%% Load data
data = readtable('C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1_Reward_DataForPeakAnalysis.xlsx');

%%
% Find groups
% 'Time_bin', 'Dopamine'

index_3s  = find(data.Time_bin > 0 & data.Time_bin < 3);
data_3s = data(index_3s, :);

peaks = data_3s(:, {'Genotype','Mouse','Session','Channel','subject','RewardNo'});
[G_peaks, peaks] = findgroups(peaks);

%Create smoothing filter and apply
smooth3 = @(x) {smooth(x,3)};
smoothed = splitapply(smooth3, data_3s.Dopamine, G_peaks);
smoothed = vertcat(smoothed{:});
data_3s.smooth3 = smoothed;


%find peaks
[peaks.peakVal, peaks.peakTime] = splitapply(@max, data_3s.Dopamine, G_peaks);
[peaks.smoothPeakVal, peaks.smoothPeakTime] = splitapply(@max, data_3s.smooth3, G_peaks);


writetable(peaks, 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1_Rewards_PeaksAnalysis.xlsx');


