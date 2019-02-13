function [Mean, Median, SEM, N] = aggregate(data, groups)

% data = vector with subjects as columns [can't handle multiple columns,
%   average prior to using this function!
%
% groups = cell array with a group represented as a column, each cell
%   containing a vector of column numbers associated with the subjects in
%   that group

Mean = [];
Median = [];
SEM = [];
N = [];

for i =  1:size(groups,2)
    Mean(i) = mean(data(1,groups{i}));
    Median(i) = median(data(1,groups{i}));
    N(i) =  size(groups{i},2);
    SEM(i) =  std(data(1,groups{i}))/(sqrt(N(i)));
end
