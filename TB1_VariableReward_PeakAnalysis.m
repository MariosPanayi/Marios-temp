%Optional Reset
Reset = 1;
    if Reset
    clear all
    close all
    clc
    end
    %%
load('C:\Users\Marios\OneDrive\GluA1_Data\TB1_VariablerewardData\TB1sandersonvariablerewarddata4841fullanimalsV2.mat');
%%

index_3s  = find(data.Time_bin > 0 & data.Time_bin < 3);
data_3s = data(index_3s, :);

peaks = data_3s(:, {'uid', 'Genotype','Mouse','Session','Channel','subject','RewardNo', 'RewardSize', 'nanPercent10s_post_reward','RewardTime'});
[G_peaks, peaks] = findgroups(peaks);

%Create smoothing filter and apply
smooth3 = @(x) {smooth(x,3)};
smoothed = splitapply(smooth3, data_3s.Dopamine, G_peaks);
smoothed = vertcat(smoothed{:});
data_3s.smooth3 = smoothed;


%find peaks
[peaks.peakVal, peaks.peakTime] = splitapply(@max, data_3s.Dopamine, G_peaks);
[peaks.smoothPeakVal, peaks.smoothPeakTime] = splitapply(@max, data_3s.smooth3, G_peaks);


writetable(peaks, 'C:\Users\Marios\OneDrive\GluA1_Data\TB1_VariablerewardData\TB1_VariableRewards_PeaksAnalysis.xlsx');
