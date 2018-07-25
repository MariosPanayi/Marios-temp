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
smooth3 = @(x) {smooth(x,18)};
smoothed = splitapply(smooth3, T.Distance,G4);
%Unpack the cell array then attach to dataTable
smoothed = vertcat(smoothed{:});
T.smooth3 = smoothed;


%Saving a table
writetable(T,'C:\Users\mpanagi\Documents\GitHub\Marios-temp\MP009_Layla\AABsmoothed.xlsx')


%summaryData.peak60s = splitapply(@mean, T.Distance, G4);




