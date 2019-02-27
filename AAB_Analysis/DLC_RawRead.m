function [data, bodyparts] = DLC_RawRead(filename)
% filename = csv file from DLC analysis
% Reads csv data file, throws away header
% first column i s frame number (starting at 0), then sets of 3 columns for
% each body part with x,y coords and then the confidence p-value of the model
% Also reads header and extracts all body part labels
startRow = 4;
data = csvread(filename,startRow);
[~,txt,~] = xlsread(filename);
labels = txt(2,:);
bodyparts = labels(2:3:size(labels,2));
