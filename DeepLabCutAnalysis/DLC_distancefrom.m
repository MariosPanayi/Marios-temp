function [distance] = DLC_distancefrom(DLC_data, fixed)
%DLC_distancefrom() calculated the distance from a fixed point of each body
%part in a DLC frame
% DLC_data = data in DLC format i.e. numeric matrix with no headers,
%   first column is frame number,
%   then groups of 3 columns for each tracked part (x,y,confidence
%
% fixed = [x,y] co-ordinates of a fixed point to compare body parts with

numparts = (size(DLC_data, 2) - 1)/3;
numrows = size(DLC_data,1);

for i = 1:numparts
    %identify column number for x, y, confidence for each body part (i)
    x = 1 + (i*3) - 2;
    y = 1 + (i*3) - 1;
    confidence = 1 + (i*3);
    %Array of distance travelled by each body part as a column
    distance(:,i) =  sqrt((DLC_data(:,x) - fixed(1)).^2 + (DLC_data(:,y)- fixed(2)).^2);
end
end
