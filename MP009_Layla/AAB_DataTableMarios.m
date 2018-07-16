

%make datatable easier to call
T = MP009LaylaMiceAABDataS1;

%create a secondary Table that will act as a sub-table
%Note that we are pulling out all the levels we want to specify here as groups/grouping levels
databins = T(:, {'Genotype','Animal', 'Stage', 'minbins'});


%Here we find what the unique groups are based on the groups/grouping variables we left in databins
%G will acxt as a sparse array for the split apply functions
%databins will be replaced with a list of all the unqiue occurences in the order correspodnign to sparse array G
[G,databins] = findgroups(databins);

databins.distance = splitapply(@mean, T.Distance, G);
databins.std = splitapply(@std, T.Distance, G);
databins.sumDistance = splitapply(@sum, T.Distance, G);
databins.size = splitapply(@size, T.Distance, G);
databins.sem = databins.std/(sqrt(databins.size(1)));


%define inline function with 2 arguments to get the standard error of the mean
%sem = @(std,n) (std/sqrt(n-1))


%pull out first min
firstminbin = databins(:, {'Genotype','Animal', 'Stage', 'minbins'});
[G2,firstminbin] = findgroups(firstminbin.minbins == 1);
firstminbin.distance = splitapply(databins.distance,G2) 

[a,b] = findgroups(databins.minbins == 1)