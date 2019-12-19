function [occupancy] = DLC_bodypartsinpolygon(DLC_data, polygonx, polygony)
% Determines if xy co-ordinates are inside a specified polygon
% DLC_data = data in DLC format i.e. numeric matrix with no headers,
%   first column is frame number,
%   then groups of 3 columns for each tracked part (x,y,confidence)
% polygon, polygony are the x and y co-ordinates for the polygon of interest
numparts = (size(DLC_data, 2) - 1)/3;
numrows = size(DLC_data,1);

for i = 1:numparts
    %identify column number for x, y, confidence for each body part (i)
    x = 1 + (i*3) - 2;
    y = 1 + (i*3) - 1;
    confidence = 1 + (i*3);
    %Array of logical with each body part as a column and a logical
    %indicating whether the part is located inside the polygon
    occupancy(:,i) =  inpolygon(DLC_data(:,x), DLC_data(:,y), polygonx, polygony);
end
