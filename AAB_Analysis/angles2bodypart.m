function [polar, angleChange] = angles2bodypart(target,pivot)
% input 2 matrices (col1,col2 -> x,y, rows = time) of a target body part and another pivot body part you want to
% analyse the rotation around.

% center target relative to pivot
centered = target-pivot;
% Calculate the polar co-ordinates of the target relative to the pivot
polar = atan2(centered(:,1),centered(:,2));
% Calculate change in angle between successive timepoints
% Unwrap function minimises dicontinuities in the radian/polar co-ordinate
% scale
angleChange = diff(unwrap(polar));

% polar & angleChange outoput is in radians, use radtodeg to convert to degrees
% If angleChange is positive -> clockwise rotation
% Of angle change is negative -> anti-clockwise rotation


