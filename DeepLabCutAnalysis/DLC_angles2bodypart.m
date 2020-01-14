function [polar, angleChange] = DLC_angles2bodypart(DLC_data, reference_pos)
% input 2 matrices (col1,col2 -> x,y, rows = time) of a target body part and another pivot body part you want to
% analyse the rotation around.

% reference_pos = position number of the DLC labelled bodypart to use as
% the reference point i.e. if you want to use the 4th labelled body part
% then reference_pos = 4

% polar & angleChange outoput is in radians, use radtodeg to convert to degrees
% If angleChange is positive -> clockwise rotation
% Of angle change is negative -> anti-clockwise rotation

numparts = (size(DLC_data, 2) - 1)/3;
numrows = size(DLC_data,1);

for i = 1:numparts
    %identify column number for x, y, confidence for each body part (i) -
    
    x1 = 1 + (i*3) - 2;
    y1 = 1 + (i*3) - 1;
    confidence1 = 1 + (i*3);
    
    % position (j) of the reference part
    x2 = 1 + (reference_pos*3) - 2;
    y2 = 1 + (reference_pos*3) - 1;
    confidence2 = 1 + (reference_pos*3);
    
    % center target relative to pivot
    centered = [DLC_data(:,x1),DLC_data(:,y1)] - [DLC_data(:,x2),DLC_data(:,y2)];
    % Calculate the polar co-ordinates of the target relative to the pivot
    polar(:,i) = atan2(centered(:,1),centered(:,2));
    % Calculate change in angle between successive timepoints
    % Unwrap function minimises dicontinuities in the radian/polar co-ordinate scale
    angleChange(:,i) = diff(unwrap(polar(:,i)));
end
end





% polar & angleChange outoput is in radians, use radtodeg to convert to degrees
% If angleChange is positive -> clockwise rotation
% Of angle change is negative -> anti-clockwise rotation


