%% AAB MP003 Deep Lab Cut Analysis
% MP003 Test 1
% For 524 x 520 videos 342 px/40cm
% For 576 x 768 videos 328 px/40cm
% MP004_SameDiff_02mgkg
% For 576 x 768 videos 315 px/40cm

%% Load in files contining video params
% videoattributes.csv from VideoFileAttributes_extract.m function
% {'name'}    {'fps'}    {'height'}    {'width'}    {'Duration'}

% vid = readtable('F:\Marios 2017\MP003_SameDiff1_VideoConversion\videoattributes.csv');

vid = readtable('F:\DLC_AllVideos_Analysis\MP004_MK801\videoattributes.csv');
% Video_SubjNumbers.csv created from excel output from ANYMaze containing
% video file numbers and subsequent animal name/genotype/grouping
% {'x___Test'}    {'Animal'}    {'Treatment'}    {'Stage'}

% id = readtable('F:\Marios 2017\MP003_SameDiff1_VideoConversion\Video_SubjNumbers.csv','Delimiter',',');
id = readtable('F:\DLC_AllVideos_Analysis\MP004_MK801\Video_SubjNumbers.csv','Delimiter',',');



%% Load Data
% i = 1: size(vid.name,1)

RawData = struct;
k = 0;
for i = 1: size(vid.name,1)
    if sum(strcmp(id.x___Test,vid.name(i)))
        k = k+1;
        path = 'F:\DLC_AllVideos_Analysis\MP004_MK801\';
        filename = vid.name(i);
        filename = char(filename);
        filename = filename(1:(length(filename)-4));
        fileextension = 'DeepCut_resnet50_AABFeb18shuffle1_870000.csv';
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

%save('C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\MK801_Data','RawData')
%% Once above steps have been run once, just run from this point onwards to minimise analysis time


% load('C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\MP003_Data.mat')
% load('C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\MK801_Data.mat')
% SP3 path
load('C:\Users\Marios\Documents\GitHub\Marios-temp\AAB_Analysis\MK801_Data.mat')


% Options
interpolate = 1; % Interpolate between co-ordinates with low confidence i.e. < 100%
anymazeEmpty = 1; % If the first 5s of the video has an empty image from anymaze

crop = 1;
crop_fromtracking = 1;
crop_xy = [200,200,570,570; 125,480,125,480]; %xy co-ordinates of a rectangle in the pixel space of the video. Any values outside of this will be considered NaNs

% Conversion of px to cm, determine from box tracking?
auto_conversion = 1;

% Order of labeled body part columns in raw data
nose = 1;
tail = 2;
rightear = 3;
leftear = 4;
boxTopLeft = 5;
boxTopRight = 6;
boxBottomLeft = 7;
boxBottomRight = 8;
innerTopLeft = 9;
innerTopRight = 10;
innerBottomLeft = 11;
innerBottomRight = 12;
%composite parts - number to continue from last DLC part above
avg = 13;
head = 14;
midear = 15;
avg2pts = 16;

%summary of all var names
var_names = {'nose', 'tail', 'rightear', 'leftear', 'boxTopLeft', 'boxTopRight', 'boxBottomLeft', 'boxBottomRight', 'innerTopLeft', 'innerTopRight', 'innerBottomLeft', 'innerBottomRight', 'avg', 'head', 'midear', 'avg2pts'};
vars = [nose tail rightear leftear boxTopLeft boxTopRight boxBottomLeft boxBottomRight innerTopLeft innerTopRight innerBottomLeft innerBottomRight avg head midear avg2pts];

% composite parts - components to average
avg_comp = [nose tail rightear leftear];
head_comp = [nose rightear leftear];
midear_comp = [rightear leftear];
avg2pts_comp = [head tail];
%list any composites here in separate rows
composites_comp = {avg_comp; head_comp; midear_comp; avg2pts_comp};

%
parts = [nose tail rightear leftear];
boxlimits = [boxTopLeft boxTopRight boxBottomLeft boxBottomRight];
composites = [avg, head, midear, avg2pts];



% Order of sub columns from Raw data
x = 1;
y = 2;
likelihood = 3;

% initialise vars
full_labels = {};
full_data = [];
Smooth_data = RawData;
    for i = 1:size(RawData,2) 
    
    dataraw = RawData(i).data;
    
    % path = 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\';
    % filename = 'Test 1DeepCut_resnet50_AABFeb18shuffle1_225000.csv';
    % [dataraw, bodyparts]  = DLC_RawRead([path,filename]);
    
    % remove first ~5s of data when still frame is in the video
    fps = RawData(1).fps; %frame rate of video in frames per second
    if anymazeEmpty
        emptyframes = ceil(5*fps);
        dataraw = dataraw(emptyframes:end,:);
    end
    % Additional step to ensure tracking starts at the first moment DLC is
    % 100% certain of the presence of a mouse
    [data, cutoff_frame] = DLC_prunestart(dataraw, parts, likelihood);
    
    
    % Determine crop co-ordinates from tracking or manual input values    
    if crop_fromtracking
        
        for k = 1:length(boxlimits)
            crop_xy(:,k) = [median(smooth(data(:,[(boxlimits(k)-1)*3 + x + 1])));median(smooth(data(:,[(boxlimits(k)-1)*3 + y + 1])))];
        end
        min_xy = [min(crop_xy(x,:)),min(crop_xy(y,:))];
        max_xy = [max(crop_xy(x,:)),max(crop_xy(y,:))];
    else
        min_xy = [min(crop_xy(x,:)),min(crop_xy(y,:))];
        max_xy = [max(crop_xy(x,:)),max(crop_xy(y,:))];
    end
    
    % Use cropping values to identify points outside the box and label as nan
    if crop
      for j = [(parts-1)*3 + x + 1]
          data(find(data(:,j) < min_xy(x)),j) = nan;
          data(find(data(:,j) > max_xy(x)),j) = nan;
      end
      for j = [(parts-1)*3 + y + 1]
          data(find(data(:,j) < min_xy(y)),j) = nan;
          data(find(data(:,j) > max_xy(y)),j)= nan;
      end      
    end
    
    % Interpolate any time points during session that do not have 100 certainty
    if interpolate
        criterion = 1;
        [data] = interpolateLowConfidence(data,criterion);
    end
    
        
     % Create composite scores out fof body parts
    [data] = appendcomposites(data, composites, composites_comp);
    
        
    % Convert xy co-ordinates into distance travelled (pixels)
    smoothxy = .25*fps;
    [Distance, Confidence, framenum, Distance_labels, X_raw, Y_raw, X_Smoothed, Y_Smoothed] = distanceTravelled(data, smoothxy, vars, var_names);
    
% Manual override of auto conversion?
    % MP003 Test 1
    % For 524 x 520 videos 342 px/40cm
    % For 576 x 768 videos 328 px/40cm
    % MP004_SameDiff_02mgkg
    % For 576 x 768 videos 315 px/40cm
    
    
    % calculate average difference in position between tracked corners
    if auto_conversion
        conversion(i) = mean([abs(median(X_Smoothed(:,innerTopLeft)) - median(X_Smoothed(:,innerBottomLeft))),
            abs(median(X_Smoothed(:,innerTopRight)) - median(X_Smoothed(:,innerBottomRight))),
            abs(median(Y_Smoothed(:,innerTopLeft)) - median(Y_Smoothed(:,innerTopRight))),
            abs(median(Y_Smoothed(:,innerBottomLeft)) - median(Y_Smoothed(:,innerBottomRight)))]);
        Distance = Distance./(conversion(i)/40);
    elseif RawData(1).vidwidth == 520
        Distance = Distance./(342/40);
    elseif RawData(1).vidwidth == 768
        Distance = Distance./(328/40);
    end
    
    % Discretize the data into time bins (need to know the frame rate of the video for this)
    
    fps = RawData(i).fps; %frame rate of video in frames per second
    bin_duration = 5; %in seconds
    framesSmoothing = 1;
    
    [Distance_Bins] = distanceBins(Distance, fps, bin_duration, framesSmoothing);
    
    % Add data to a data table
    % identify location of corresponding video from vid.name in id.x___Test
    % id {'x___Test'}    {'Animal'}    {'Treatment'}    {'Stage'}
    
    % nrows = number of data rows
    nrows = size(Distance_Bins,1);
    
    % use repmat to repeat test info for each row of DIstance data [i.e. data in long format]
    testnum = RawData(i).testnum;
    testnum = repmat(testnum, nrows,1);
    animal = RawData(i).animal;
    animal = repmat(animal, nrows,1);
    treatment = RawData(i).treatment;
    treatment = repmat(treatment,nrows, 1);
    stage = RawData(i).stage;
    stage = repmat(stage, nrows,1);
    
    % combine all the labels and the data
    clear templabels tempdata
    templabels = {testnum, animal, treatment, stage};
    tempdata =  Distance_Bins;
    full_labels(i,:) = {templabels};
    full_data = [full_data; tempdata];
    
    Smooth_data(i).rawX = X_raw;
    Smooth_data(i).rawY = Y_raw;
    
    Smooth_data(i).smoothX = X_Smoothed;
    Smooth_data(i).smoothY = Y_Smoothed;
 
end
%

% Exapnd cell array of labels into rows
dummy = vertcat(full_labels{:});
% split individual columns of labels 
testnum = vertcat(dummy{:,1});
animal = vertcat(dummy{:,2});
treatment = vertcat(dummy{:,3});
stage = vertcat(dummy{:,4});
T1 = table(testnum,animal,treatment,stage);

% add data to Table and add column labels
table_names = var_names;
table_names(end+1) = {'Bins'};
T1 = addvars(T1, full_data);
T1 = splitvars(T1, 'full_data','NewVariableNames', table_names);

%% Angle analysis

parts = [nose tail rightear leftear];
boxlimits = [boxTopLeft boxTopRight boxBottomLeft boxBottomRight];
composites = [avg, head, midear, avg2pts];

Smooth_data(i).rawX


% Identify average location in 30s bins
bin_duration = 30;
fps = RawData(1).fps;
binwidth = fps*bin_duration;
bins = ceil([1:size(Distance,1)]/binwidth)';




%  hist3([Smooth_data(8).rawX(:,avg),Smooth_data(8).rawY(:,avg)], 'CDataMode','auto','FaceColor','interp');
%  view(2)

% histogram2(Smooth_data(8).rawX(:,avg),Smooth_data(8).rawY(:,avg), 'FaceColor', 'flat');
% view(2)

%% Optional plotting - removed from main loop
% plotDiagnostics_path = 0; % Diagnostic plots of each video to assess tracking
% plotDiagnostics_corr = 0; % Diagnostic plots of each video to assess outliers
%     
%     % Tracking Diagnostic Plots
%     if plotDiagnostics_path
%         figure;
%         plot(data(:,2),data(:,3))
%         hold on
%         plot(data(:,5),data(:,6))
%         plot(data(:,8),data(:,9))
%         plot(data(:,11),data(:,12))
%         legend({'Nose', 'Tail', 'LEar', 'REar'});
%         title({char({RawData(i).filename, char(RawData(i).treatment), char(RawData(i).animal), char(RawData(i).stage)})});
%         hold off
%     end
%     
%     if plotDiagnostics_corr
%         figure;
%         title({char({RawData(i).filename, char(RawData(i).treatment), char(RawData(i).animal), char(RawData(i).stage)})});
%         subplot(1,2,1)
%         scatter(data(:,2), data(:,5));
%         hold on
%         scatter(data(:,2), data(:,8));
%         scatter(data(:,2), data(:,11));
%         scatter(data(:,5), data(:,8));
%         scatter(data(:,5), data(:,11));
%         scatter(data(:,8), data(:,11)); 
%         hold off
% 
%         subplot(1,2,2)
%         scatter(data(:,3), data(:,6));
%         hold on
%         scatter(data(:,3), data(:,9));
%         scatter(data(:,3), data(:,12));
%         scatter(data(:,6), data(:,9));
%         scatter(data(:,6), data(:,12));
%         scatter(data(:,9), data(:,12));
%         hold off              
%     end
%     
%  