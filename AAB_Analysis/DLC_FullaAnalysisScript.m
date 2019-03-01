%% AAB MP003 Deep Lab Cut Analysis
% For 524 x 520 videos 342 px/40cm
% For 576 x 768 videos 328 px/40cm
%% Load in files contining video params
% videoattributes.csv from VideoFileAttributes_extract.m function
% {'name'}    {'fps'}    {'height'}    {'width'}    {'Duration'}
vid = readtable('F:\Marios 2017\MP003_SameDiff1_VideoConversion\videoattributes.csv');

% Video_SubjNumbers.csv created from excel output from ANYMaze containing
% video file numbers and subsequent animal name/genotype/grouping
% {'x___Test'}    {'Animal'}    {'Treatment'}    {'Stage'}
id = readtable('F:\Marios 2017\MP003_SameDiff1_VideoConversion\Video_SubjNumbers.csv','Delimiter',',');



%% Load Data
% i = 1: size(vid.name,1)

RawData = struct;
k = 0;
for i = 1: size(vid.name,1)
    if sum(strcmp(id.x___Test,vid.name(i)))
        k = k+1;
        path = 'F:\Marios 2017\MP003_SameDiff1_VideoConversion\DLC_Tracking_FullNetwork\';
        filename = vid.name(i);
        filename = char(filename);
        filename = filename(1:(length(filename)-4));
        fileextension = 'DeepCut_resnet50_AABFeb18shuffle1_1030000.csv';
        [dataraw, bodyparts]  = DLC_RawRead([path,filename,fileextension]);
        RawData(k).filename = filename;
        RawData(k).data = dataraw;
        RawData(k).bodyparts = bodyparts;
        RawData(k).vidwidth = vid.width(i);
        RawData(k).fps = vid.fps(i);
        j = strcmp(id.x___Test,vid.name(i));
        RawData(k).testnum = id.x___Test(j);
        RawData(k).animal = id.Animal(j);
        RawData(k).treatment = id.Treatment(j);
        RawData(k).stage = id.Stage(j);
    end
end

%save('C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\MP003_Data',RawData)
%% Once above steps have been run once, just run from this point onwards to minimise analysis time


load('C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\MP003_Data.mat')
full_labels = {};
full_data = [];

for i = 1:size(RawData,2)
    
    dataraw = RawData(i).data;
    
    % path = 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\';
    % filename = 'Test 1DeepCut_resnet50_AABFeb18shuffle1_225000.csv';
    % [dataraw, bodyparts]  = DLC_RawRead([path,filename]);
    
    % remove first ~5s of data when still frame is in the video
    
    [data, cutoff_frame] = DLC_prunestart(dataraw);
    
    % Convert xy co-ordinates into distance travelled (pixels)
    smoothxy = 5;
    [Distance, Confidence, framenum, Distance_labels] = distanceTravelled(data, smoothxy);
    
    % convert pixels into cm
    % For 524 x 520 videos 342 px/40cm
    % For 576 x 768 videos 328 px/40cm
    if RawData(1).vidwidth == 520
        Distance = Distance./(342/40);
    elseif RawData(1).vidwidth == 768
        Distance = Distance./(328/40);
    end
    
    % Discretize the data into time bins (need to know the frame rate of the video for this)
    
    fps = RawData(1).fps; %frame rate of video in frames per second
    bin_duration = 5; %in seconds
    framesSmoothing = 10*fps;
    
    [Distance_Bins] = distanceBins(Distance, fps, bin_duration, framesSmoothing);
    
    % Add data to a data table
    %identify location of corresponding video from vid.name in id.x___Test

    % id {'x___Test'}    {'Animal'}    {'Treatment'}    {'Stage'}
    nrows = size(Distance_Bins,1);
    
    testnum = RawData(i).testnum;
    testnum = repmat(testnum, nrows,1);
    animal = RawData(i).animal;
    animal = repmat(animal, nrows,1);
    treatment = RawData(i).treatment;
    treatment = repmat(treatment,nrows, 1);
    stage = RawData(i).stage;
    stage = repmat(stage, nrows,1);
    
    
    clear templabels tempdata
    
    templabels = {testnum, animal, treatment, stage};
    tempdata =  Distance_Bins;
    full_labels(i,:) = {templabels};
    full_data = [full_data; tempdata];
end
%

dummy = vertcat(full_labels{:});

testnum = vertcat(dummy{:,1});
animal = vertcat(dummy{:,2});
treatment = vertcat(dummy{:,3});
stage = vertcat(dummy{:,4});
nose = full_data(:,1);
tail = full_data(:,2);
LEar = full_data(:,3);
REar = full_data(:,4);
avg = full_data(:,5);
head = full_data(:,6);
Bins = full_data(:,7);

T1 = table(testnum,animal,treatment,stage,Bins,nose,tail,LEar,REar,avg,head);




% for breaking up the cell array at the end use this in combination with another column wise vertcat
% vertcat(full_labels{:})
%% Optional plotting
% nose = 1;
% tail = 2;
% LEar = 3;
% REar = 4;
% avg = 5;
% head = 6;
%
% plotpart = [head, avg, tail];
%
% figure
% for i = plotpart
%     plot(Distance(:,i))
%     hold on
% end
% legend(Distance_labels{1,plotpart})
% hold off
%
% figure
% for i = plotpart
%     plot(Distance_Bins(:,i))
%     hold on
% end
% legend(Distance_labels{1,plotpart})
% hold off
%
