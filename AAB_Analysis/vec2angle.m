function [length, angle_rad, angle_deg] = vec2angle(v,w)
% Determines the angle between two vectors [x and y in cols 1 and 2]
% For multiple points, put in different rows
% length provides the length fo the vectors
% Remember to center vectors to an appropriate point before calulating angle and lengths
% Direction indicates whether the angle between vectors vs and w is
% clockwise or anticlockwise i.e. direction when going from v -> w

length = [vecnorm(v,2,2), vecnorm(w,2,2)];
angle_deg = acosd(dot(v,w,2)./(vecnorm(v,2,2).*vecnorm(w,2,2)));
angle_rad = acos(dot(v,w,2)./(vecnorm(v,2,2).*vecnorm(w,2,2)));
