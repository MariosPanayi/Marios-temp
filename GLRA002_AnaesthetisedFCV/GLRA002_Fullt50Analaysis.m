% GLRA002_Fit 250 to all traces
%% 01Baseline1
load('F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_01BaselineData.mat');

tempData = [];

%for i = 1: size(data,2)
for i = 3
    %convert data to numeric matrix, rows 1,3,5... are DA, rows 2,4,6.... are pH
    tempData = cell2mat(data(i).processed.c_predicted(1,:)');
    %DA data picker for even rows
    tempData = tempData(1:2:size(tempData,1),:);
    %Save all trials
    tempData = tempData(1:end,:);
    
    
    %Apply calibration factor to DA data
    temp_Data_Cal = tempData./data(i).calibrationFactor;
    
    for j = 1:size(temp_Data_Cal,1)
        % find t50 and save output
        [t50, rsq, a, b, Y, pks, pklocs, MaxFitIndex] = t50Find(temp_Data_Cal(j,:));
        
        data(i).T50Analysis.t50(j) = t50;
        data(i).T50Analysis.rsq(j) = rsq;
        data(i).T50Analysis.a(j) = a;
        data(i).T50Analysis.b(j) = b;

        data(i).T50Analysis.pks(j) = pks;
        data(i).T50Analysis.pklocs(j) = pklocs;
        data(i).T50Analysis.MaxFitIndex(j) = MaxFitIndex;
    end
    tempData = [];
end












