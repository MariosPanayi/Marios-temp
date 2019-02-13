
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

%% Plot using data aggregator function
%Plot means of first 30s data


[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(sum(data1(1:6,:),1), groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(sum(data2(1:6,:),1), groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(sum(data3(1:6,:),1), groups);



%%
limits = [-1 1];
Units = 'Distance (30s)';


figure

subplot(1,3,1)
bar(figLabels, Mean(1,:))
hold on
errorbar(figLabels, Mean(1,:), SEM(1,:), '.')
title('Trial1')
ylabel(Units)
ylim(limits)
hold off

subplot(1,3,2)
bar(figLabels, Mean(2,:))
hold on
errorbar(figLabels, Mean(2,:), SEM(2,:), '.')
title('Trial2')
ylim(limits)
hold off

subplot(1,3,3)
bar(figLabels, Mean(3,:))
hold on
errorbar(figLabels, Mean(3,:), SEM(3,:), '.')
title('Trial3')
ylim(limits)
hold off



figure

i = 1;
subplot(2,3,1)
bar(Mean(:,i))
hold on
errorbar(Mean(:,i), SEM(:,i), '.')
title(groupLabels(i))
ylabel(Units)
ylim(limits)
hold off

i = 2;
subplot(2,3,4)
bar(Mean(:,i))
hold on
errorbar(Mean(:,i), SEM(:,i), '.')
title(groupLabels(i))
ylabel(Units)
ylim(limits)
hold off

i = 3;
subplot(2,3,2)
bar(Mean(:,i))
hold on
errorbar(Mean(:,i), SEM(:,i), '.')
title(groupLabels(i))
ylim(limits)
hold off

i = 4;
subplot(2,3,5)
bar(Mean(:,i))
hold on
errorbar(Mean(:,i), SEM(:,i), '.')
title(groupLabels(i))
ylim(limits)
hold off

i = 5;
subplot(2,3,3)
bar(Mean(:,i))
hold on
errorbar(Mean(:,i), SEM(:,i), '.')
title(groupLabels(i))
ylim(limits)
hold off

i = 6;
subplot(2,3,6)
bar(Mean(:,i))
hold on
errorbar(Mean(:,i), SEM(:,i), '.')
title(groupLabels(i))
ylim(limits)
hold off
%%

range = [1:12]';
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

%% 
range = [1:18]';
for i = 1: size(data1,2)
coeffs = polyfit(range,cumsum(data1(range,i),1), 2);
intercept1(i) = coeffs(3);
slope1(i) = coeffs(2);
slopequad1(i) = coeffs(1);

coeffs = polyfit(range,cumsum(data2(range,i),1), 2);
intercept2(i) = coeffs(3);
slope2(i) = coeffs(2);
slopequad2(i) = coeffs(1);

coeffs = polyfit(range,cumsum(data3(range,i),1), 2);
intercept3(i) = coeffs(3);
slope3(i) = coeffs(2);
slopequad3(i) = coeffs(1);
end

[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(slope1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(slope2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(slope3, groups);
%%

[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(slopequad1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(slopequad2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(slopequad3, groups);

%%
[Mean(1,:), Median(1,:), SEM(1,:), ~] = aggregate(intercept1, groups);

[Mean(2,:), Median(2,:), SEM(2,:), ~] = aggregate(intercept2, groups);

[Mean(3,:), Median(3,:), SEM(3,:), ~] = aggregate(intercept3, groups);

