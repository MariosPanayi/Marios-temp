function [Data] = interpolateLowConfidence(data,criterion)

% Nose
Nose = data(:,4) < criterion;
data(Nose, [2,3]) = nan;
% Tail
Tail = data(:,7) < criterion;
data(Tail, [5,6]) = nan;
% LEar
LEar = data(:,10) < criterion;
data(LEar, [8,9]) = nan;
% REar
REar = data(:,13) < criterion;
data(REar, [11,12]) = nan;

Data = fillmissing(data, 'linear', 1);