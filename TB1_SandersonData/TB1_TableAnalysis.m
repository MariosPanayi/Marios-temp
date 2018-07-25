%Load data
load('C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1DataLongFormat.mat');
%simplify
data = TB1paramswithextrainfo100nan300300reward4841S1;

%Table Colnames
%{'uid', 'Genotype', 'Mouse', 'Session', 'Channel', 'Time_bin', 'Dopamine', 'HouseLED', 'PreviousSameDiff', 'NextSameDiff', 'FirstSecond', 'subject', 'LightNo', 'nans','rewardTTL'} 

data.base5s = data.Time_bin > -5 & data.Time_bin < 0;
data.base5s = data.base5s .* data.Dopamine;

%Identify each individual trial within each animal/channel/session etc...
%to analyze individually. Note that Time_bin is left out to allow analysis
%of data within the trials
datatrials = data(:, {'uid','subject', 'Mouse', 'Session', 'Channel', 'Genotype', 'LightNo', 'HouseLED', 'FirstSecond', 'PreviousSameDiff', 'NextSameDiff'});
[G,datatrials] = findgroups(datatrials);





%Baseline Subtracted based on new mean
meansub = @(x,y) {x - nanmean(y)};

meanbaseline_data = splitapply(meansub, data.Dopamine, data.base5s, G);
meanbaseline_data = vertcat(meanbaseline_data{:});
data.baselined5s = meanbaseline_data;




% writetable(data, 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\TB1DataLongFormat_test.xlsx');



%%





%To apply complex functions you may need to ouput the data as a cell array
%then unpack the cellarray before attaching it to the table
%define smooth function and identify out put as cells {}
smooth3 = @(x) {smooth(x,18)};
smoothed = splitapply(smooth3, T.Distance,G4);
%Unpack the cell array then attach to dataTable
smoothed = vertcat(smoothed{:});
T.smooth3 = smoothed;





% 
% [pks,locs,w,p] = findpeaks(data)
% 
% findpeaks(smooth(A(:,2), 5), 'MinPeakProminence', 0.05,  'Annotate','extents')