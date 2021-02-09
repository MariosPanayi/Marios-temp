function [filtered, percentNaNs] = DLC_interpolateLowConfidence(data,criterion)
%% Interpolates values in DLC tracking if confidence in the estimate is below a threshold
% data = data in DLC format i.e. numeric matrix with no headers,
%   first column is frame number,
%   then groups of 3 columns for each tracked part (x,y,confidence)
%
% criterion = Threshold cutoff for confidence e.g. 0.95 any points with
%   confidence values lower than .95 will be interpolated across

%
numparts = (size(data, 2) - 1)/3;
numrows = size(data,1);

% copy data to new variabel to save filtered values
filtered = data;

%
for i = 1:numparts
    %identify column number for x, y, confidence for each body part (i)
    x = 1 + (i*3) - 2;
    y = 1 + (i*3) - 1;
    confidence = 1 + (i*3);
    %Indices of rows with sub-threshold confidence
    lowConfidence = find(data(1:numrows,confidence) < criterion);
    filtered(lowConfidence, x:y) = nan;

    %Calculate percent NaNs in each column
    percentNaNs(i) = (sum(isnan(filtered(:,x)))/length(filtered(:,x)))*100;
end


%Fill nan values with linear interpolation
%note that you can easily change this to other forms of fill using options
%in fillmissing() e.g. use previous non-missing etc...
filtered = fillmissing(filtered, 'linear',1, 'EndValues', 'next');