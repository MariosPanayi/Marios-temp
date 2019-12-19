function [data, bodyparts] = DLC_RawRead(filename)
% filename = csv filepath from DLC analysis e.g.
% 'E:\MK001_MK002_VideosCombinedforAnalysis\DLC\1021_G_Sanderson_1DLC_resnet50_MK001_SandersonNov1shuffle1_1030000.csv'
% Reads csv data file, throws away header
% first column is frame number (starting at 0), then sets of 3 columns for
% each body part with x,y coords and then the confidence p-value of the model
%%
% Extract numeric data
data = readmatrix(filename);

% Extract header labels with DLC labeled part names
temp_labels = readtable(filename);
header = temp_labels{1,:};
% Extract DLC part labels in order
bodyparts = {header{2:3:size(header,2)}};

%% Old code that can be used pre matlab 2019a installations? For some reason xlsread is quite buggy in 2019a. 
% Solutions above are 2019a compativle
% startRow = 4;
% data = csvread(filename,startRow);
% [~,txt,~] = xlsread(filename);
% % Also reads header and extracts all body part labels
% labels = txt(2,:);
% bodyparts = labels(2:3:size(labels,2));
