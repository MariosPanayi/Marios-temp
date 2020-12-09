function [downsampled] = DLC_downsample(DLC_data, fps, downsamplerate)
%DLC_distancefrom() calculated the distance from a fixed point of each body
%part in a DLC frame
% DLC_data = data in DLC format i.e. numeric matrix with no headers,
%   first column is frame number,
%   then groups of 3 columns for each tracked part (x,y,confidence
%
% fps = fps of the video
% downsamplerate = desired final sample rate e.g. for 10Hz downsamplerate = 10

totalframes = size(DLC_data,1);
index = round([1:fps/downsamplerate:totalframes]);
downsampled = DLC_data(index,:);

