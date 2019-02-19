
%% Load data
path = 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\';
filename = 'MP003_AAB_Sess1_Raw.xlsx';

%Extract header containing group/trial/subject info
[~,txt,~] = xlsread([path,filename]);
header = txt;

%Extract locomotor data for each sheet and create data matrices
sheet = 1;
[data1,~,~] = xlsread([path,filename], sheet);
sheet = 2;
[data2,~,~] = xlsread([path,filename], sheet);
sheet = 3;
[data3,~,~] = xlsread([path,filename], sheet);
%Data format [rows x cols] -> [timepoints x subjects]
data1 = data1';
data2 = data2';
data3 = data3';

clear sheet
%% Identify columns associated with subjects in each group
wt_same = [1:8];
wt_diff = [9:16];
het_same = [17:24];
het_diff = [25:32];
ko_same = [33:40];
ko_diff = [41:48];

groups = {wt_same,wt_diff,het_same,het_diff,ko_same,ko_diff};
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

%% Get data ready for plot using data aggregator function
%% Plot means of first 30s data


[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(sum(data1(1:6,:),1), groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(sum(data2(1:6,:),1), groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(sum(data3(1:6,:),1), groups);

limits = [0 4]; %y axis limits
YaxisLabel = 'Distance (30s)';
trials = 3; %number of trials in the test
plotpanel = [3 2]; %how to panel plots of separate groups/conditions
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

AABplot(Mean,SEM,limits,figLabels,YaxisLabel,trials,plotpanel)


%% Fit polynomials

range = [1:6]';
for i = 1: size(data1,2)
coeffs = polyfit(range,cumsum(data1(range,i),1), 1);
intercept1(i) = coeffs(2);
slope1(i) = coeffs(1);

coeffs = polyfit(range,cumsum(data2(range,i),1), 1);
intercept2(i) = coeffs(2);
slope2(i) = coeffs(1);

coeffs = polyfit(range,cumsum(data3(range,i),1), 1);
intercept3(i) = coeffs(2);
slope3(i) = coeffs(1);
end

[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(slope1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(slope2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(slope3, groups);

limits = [0 0.7]; %y axis limits
YaxisLabel = 'Linear Slope 1st order Polynomial';
trials = 3; %number of trials in the test
plotpanel = [3 2]; %how to panel plots of separate groups/conditions
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

AABplot(Mean,SEM,limits,figLabels,YaxisLabel,trials,plotpanel)

% plot intercepts
[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(intercept1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(intercept2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(intercept3, groups);

limits = [-.7 .2]; %y axis limits
YaxisLabel = 'Intercept 1st order Polynomial';
trials = 3; %number of trials in the test
plotpanel = [3 2]; %how to panel plots of separate groups/conditions
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

AABplot(Mean,SEM,limits,figLabels,YaxisLabel,trials,plotpanel)

%% Fit polynomials to raw data

range = [1:6]';
for i = 1: size(data1,2)
coeffs = polyfit(range,cumsum(data1(range,i),1), 2);
intercept1(i) = coeffs(3);
slope1(i) = coeffs(2);
quad1(i) = coeffs(1);

coeffs = polyfit(range,cumsum(data2(range,i),1), 2);
intercept2(i) = coeffs(3);
slope2(i) = coeffs(2);
quad2(i) = coeffs(1);

coeffs = polyfit(range,cumsum(data3(range,i),1), 2);
intercept3(i) = coeffs(3);
slope3(i) = coeffs(2);
quad3(i) = coeffs(1);
end


%Plot quadratic component

[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(quad1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(quad2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(quad3, groups);

limits = [-.02 0.1]; %y axis limits
YaxisLabel = 'Quadratic Curvature 2nd order Polynomial';
trials = 3; %number of trials in the test
plotpanel = [3 2]; %how to panel plots of separate groups/conditions
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

AABplot(Mean,SEM,limits,figLabels,YaxisLabel,trials,plotpanel)

%Plot linear component


[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(slope1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(slope2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(slope3, groups);

limits = [-.1 0.8]; %y axis limits
YaxisLabel = 'Linear Slope 2nd order Polynomial';
trials = 3; %number of trials in the test
plotpanel = [3 2]; %how to panel plots of separate groups/conditions
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

AABplot(Mean,SEM,limits,figLabels,YaxisLabel,trials,plotpanel)

% plot intercepts
[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(intercept1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(intercept2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(intercept3, groups);

limits = [-.7 .2]; %y axis limits
YaxisLabel = 'Intercept  2nd order Polynomial';
trials = 3; %number of trials in the test
plotpanel = [3 2]; %how to panel plots of separate groups/conditions
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

AABplot(Mean,SEM,limits,figLabels,YaxisLabel,trials,plotpanel)
%% Plot Data over time
range = [1:6]';

data1_C = cumsum(data1,1);
data2_C = cumsum(data2,1);
data3_C = cumsum(data3,1);

for i = 1:size(range,1)
   [MeanT1(i,:), MedianT1(i,:), SEMT1(i,:), ~] = aggregate(sum(data1_C(i,:),1), groups);
      [MeanT2(i,:), MedianT2(i,:), SEMT2(i,:), ~] = aggregate(sum(data2_C(i,:),1), groups); 
         [MeanT3(i,:), MedianT3(i,:), SEMT3(i,:), ~] = aggregate(sum(data3_C(i,:),1), groups);    
end

Means{1,:,:} = MeanT1;
Means{2,:,:} = MeanT2;
Means{3,:,:} = MeanT3;

SEMs{1,:,:} = SEMT1;
SEMs{2,:,:} = SEMT2;
SEMs{3,:,:} = SEMT3;


limits = [0 4]; %y axis limits
YaxisLabel = 'Distance (5s)';
trials = 3; %number of trials in the test
plotpanel = [3 2]; %how to panel plots of separate groups/conditions
groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
figLabels = reordercats(categorical(groupLabels),groupLabels);

figure
for i = 1:trials
    hold off
    subplot(1,trials,i)
    for j = 1:size(Means{i},2)
        hold on
        errorbar(range', Means{i}(:,j), SEMs{i}(:,j), '-s','CapSize',0)
    end
    title(sprintf('Trial %d', i))
    ylabel(YaxisLabel)
    ylim(limits)
    legend(groupLabels)
end

figure
for i = 1:size(groups,2)
    hold off
    subplot(plotpanel(1), plotpanel(2),i)
    for j = 1:trials
        hold on
        errorbar(range', Means{j}(:,i), SEMs{j}(:,i), '-s','CapSize',0)
        legend_temp{j} =  sprintf('Trial %d', j);
    end
    title(groupLabels{i})
    ylabel(YaxisLabel)
    ylim(limits)
    legend(legend_temp)
end





