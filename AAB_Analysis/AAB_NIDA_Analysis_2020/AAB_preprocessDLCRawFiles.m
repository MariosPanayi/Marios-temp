%% Reprocess data from scratch or load saved data?
clear all
%
folderPath = "D:\DLC_AllVideos_Analysis\Summary\";
filePath = "MK801\";


id = readtable([folderPath+filePath+"MK801_Video_SubjNumbers.csv"]);
vid = readtable([folderPath+filePath+"MK801_videoattributes.csv"]);

% Create overall data structure - combining video attributes and subject attributes into a RawData array

RawData = struct;
k = 0;
for i = 1: size(vid.name,1)
    if sum(strcmp(id.Test,vid.name(i)))
        k = k+1;
        %Combine attribute data from id and vid
        RawData(k).vidduration = vid.Duration(i);
        RawData(k).vidfps = vid.fps(i);
        RawData(k).vidheight = vid.height(i);
        RawData(k).vidwidth = vid.width(i);
        % Corresponding position within id array
        j = strcmp(id.Test,vid.name(i));
        RawData(k).vidname = id.Test(j);
        RawData(k).experiment = id.Experiment(j);
        RawData(k).animal = id.Animal(j);
        RawData(k).stage = id.Stage(j);
        RawData(k).treatment = id.Treatment(j);
        
    end    
end

% Identify the DLC output filename for each subject/trial in RawData
DLCfolder = struct2table(dir([folderPath+filePath]));
for i = 1:size(RawData, 2)
vidfilename = char(RawData(i).vidname);
vidfilename = vidfilename(1:(length(vidfilename)-4));

temp = strfind(DLCfolder.name, [vidfilename+"DLC_resnet50"]);
index = [];
for j = 1:length(temp)
if temp{j} == 1
    index = j;
end
end
RawData(i).DLCfilename = DLCfolder.name(index);
[dataraw, bodyparts]  = DLC_RawRead([folderPath+filePath+char(RawData(i).DLCfilename)]);
RawData(i).data = dataraw;
RawData(i).bodyparts = bodyparts;
end

save([folderPath+filePath+"MK801AAB_Data"],'RawData');

% Once above steps have been run once, just run from this point onwards to minimise analysis time
% else
% load("D:\DLC_AllVideos_Analysis\Summary\GluA1KO\GluA1AAB_Data.mat")


