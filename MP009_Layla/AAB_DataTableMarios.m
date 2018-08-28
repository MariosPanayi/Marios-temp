%load data

load('C:\Users\mpanagi\Documents\GitHub\Marios-temp\MP009_Layla\MP009_LaylaData_AABonly.mat');

%make datatable easier to call
T = MP009LaylaMiceAABDataS1;

%create a secondary Table that will act as a sub-table
%Note that we are pulling out all the levels we want to specify here as groups/grouping levels
databins = T(:, {'Genotype','Animal', 'Stage', 'minbins'});


%Here we find what the unique groups are based on the groups/grouping variables we left in databins
%G will acxt as a sparse array for the split apply functions
%databins will be replaced with a list of all the unqiue occurences in the order corresponding to sparse array G
[G,databins] = findgroups(databins);

databins.distance = splitapply(@mean, T.Distance, G);
databins.std = splitapply(@std, T.Distance, G);
databins.sumDistance = splitapply(@sum, T.Distance, G);
databins.size = splitapply(@size, T.Distance, G);
databins.sem = databins.std/(sqrt(databins.size(1)));


%define inline function with 2 arguments to get the standard error of the mean
%sem = @(std,n) (std/sqrt(n-1))


%pull out first min into new table
firstminbin = databins(databins.minbins ==1, :);

%group averages of first min
grp_avg_1stmin = firstminbin(:, {'Genotype', 'Stage'});
[G3,grp_avg_1stmin] = findgroups(grp_avg_1stmin);

grp_avg_1stmin.avgDistance = splitapply(@mean, firstminbin.sumDistance, G3);
grp_avg_1stmin.std = splitapply(@std, firstminbin.sumDistance, G3);
grp_avg_1stmin.size = splitapply(@size, firstminbin.sumDistance, G3);
grp_avg_1stmin.sem = grp_avg_1stmin.std/(sqrt(grp_avg_1stmin.size(1)));
grp_avg_1stmin.lowerERR = grp_avg_1stmin.avgDistance - grp_avg_1stmin.sem;
grp_avg_1stmin.upperERR = grp_avg_1stmin.avgDistance + grp_avg_1stmin.sem;

%%
%Perform computations on each mouse x trial
summaryData = T(:, {'Animal', 'Stage', 'Genotype'});
[G4,summaryData] = findgroups(summaryData);

%To apply complex functions you may need to ouput the data as a cell array
%then unpack the cellarray before attaching it to the table
%define smooth function and identify out put as cells {}
smooth3 = @(x) {smooth(x,'lowess')};
smoothed = splitapply(smooth3, T.Distance,G4);
%Unpack the cell array then attach to dataTable
smoothed = vertcat(smoothed{:});
T.smooth3 = smoothed;


% peaks = @(x) {findpeaks(x, 'MinPeakProminence', 0.05,  'Annotate','extents') } 
% [pks,locs,w,p] = findpeaks(data)
% % 
% findpeaks(smooth(A(:,2), 5), 'MinPeakProminence', 0.05,  'Annotate','extents')



datas = table2struct(T, 'ToScalar',true);



figure
hold on
subplot(2,3,1)
findpeaks(datas.smooth3(find(datas.Test == 1)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,2)
findpeaks(datas.smooth3(find(datas.Test == 2)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,3)
findpeaks(datas.smooth3(find(datas.Test == 3)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,4)
findpeaks(datas.smooth3(find(datas.Test == 4)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,5)
findpeaks(datas.smooth3(find(datas.Test == 5)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,6)
findpeaks(datas.smooth3(find(datas.Test == 6)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
hold off

figure
hold on
subplot(2,3,1)
findpeaks(datas.smooth3(find(datas.Test == 7)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,2)
findpeaks(datas.smooth3(find(datas.Test == 8)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,3)
findpeaks(datas.smooth3(find(datas.Test == 9)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,4)
findpeaks(datas.smooth3(find(datas.Test == 10)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,5)
findpeaks(datas.smooth3(find(datas.Test == 11)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
subplot(2,3,6)
findpeaks(datas.smooth3(find(datas.Test == 12)), 'MinPeakProminence', 0.05,  'Annotate','extents','WidthReference','halfheight')
hold off

%Saving a table
writetable(T,'C:\Users\mpanagi\Documents\GitHub\Marios-temp\MP009_Layla\AABsmoothed.xlsx')

%%
%Identify AAB peaks amd other statistics
%RawData - i.e. unsmoothed
data = datas.Distance(find(datas.Test == 5));
% Smooth data
smoothlowess = smooth(data, 'lowess');
smooth3 = smooth(data, 3);
smooth6 = smooth(data, 6);

maxpeak = max(data(1:18));


figure
subplot(3,1,1)
plot(data)
hold on
plot(smoothlowess)
plot(smooth3)
plot(smooth6)

legend('Raw', 'Lowess', '3 point', '6 point')
hold off

subplot(3,1,2)
findpeaks(data, 'MinPeakProminence', maxpeak*.05, 'Annotate','extents','WidthReference','halfheight')

subplot(3,1,3)

findpeaks(smoothlowess, 'MinPeakProminence', maxpeak*0.05, 'Annotate','extents','WidthReference','halfheight')

%%
data1 = datas.Distance(find(datas.Test == 10));
data2 = datas.Distance(find(datas.Test == 11));
data3 = datas.Distance(find(datas.Test == 12));
% [pks1,locs1,w1,p1] = findpeaks(data1(1:12));
% [pks2,locs2,w2,p2] = findpeaks(data2(1:12));
% [pks3,locs3,w3,p3] = findpeaks(data3(1:12));
% 
% % pks1
% % pks2
% % pks3
% % locs1
% % locs2
% % locs3
% 
% [M1,I1] = max((data1(1:18)));
% [M2,I2] = max((data2(1:18)));
% [M3,I3] = max((data3(1:18)));
% [M1, M2, M3]
% [I1, I2, I3]

order = 1;
figure
subplot(4,1,1)
hold on
fit1 = polyfit(log([1:5:30]),data1(1:6)',order);
plot(polyval(fit1, log([1:5:30])))
plot(data1(1:6))

subplot(4,1,2)
hold on
fit2 = polyfit(log([1:5:30]),data2(1:6)',order);
plot(polyval(fit2, log([1:5:30])))
plot(data2(1:6))

subplot(4,1,3)
hold on
fit3 = polyfit(log([1:5:30]),data3(1:6)',order);
plot(polyval(fit3, log([1:5:30])))
plot(data3(1:6))

subplot(4,1,4)
bar([fit1(1) fit2(1) fit3(1) fit1(2) fit2(2) fit3(2)]);

