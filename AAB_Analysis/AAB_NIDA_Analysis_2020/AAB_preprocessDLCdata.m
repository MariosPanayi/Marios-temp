%%
folderPath = "D:\DLC_AllVideos_Analysis\Summary\";
filePath = "MK801\";

load([folderPath+filePath+"MK801AAB_Data.mat"])
% folderPath = "D:\DLC_AllVideos_Analysis\Summary\";
% filePath = "GluA1KO\";

%%
% List of DLC labels in order [from RawData.bodyparts]
nose = 1;            
head1 = 2;           
head2 = 3;           
lear = 4;            
rear = 5;            
mid1 = 6;            
mid2 = 7;            
tail = 8;            
innertopleft = 9;    
innertopright = 10;   
innerbottomleft = 11; 
innerbottomright = 12;
outertopleft = 13;    
outertopright = 14;   
outerbottomleft = 15; 
outerbottomright = 16;
%composite parts - number to continue from last DLC part above
head = 17;
body = 18;
composites = [head, body];

% Separate body parts from points in apparatus
parts = [nose head1 head2 lear rear mid1 mid2 tail];
boxlimits = [outertopleft outertopright outerbottomleft outerbottomright];
boxinner = [innertopleft innertopright innerbottomleft innerbottomright];

% Create composites of body parts if desired
head_comp = [nose head1 head2 lear rear];
body_comp = [mid1 mid2 tail];
% Add these to a list of composites
composites_comp = {head_comp; body_comp};

% Order of sub columns from Raw data
x = 1;
y = 2;
likelihood = 3;

% params
params.crop = 1; % Crop data based on box limits? True or false
params.interpolate = 1; %Interpolate tracking data if below some crietrion - linear interpolation used [ see interpolateLowConfidence2.m]
params.criterion = 1; %Threshold criterion for trackign points to interpolate
params.smoothxy = 0; %0.25 %Option to smooth data with moving average over some period of time in seconds
params.boxdimension = 40; % Parameter used to convert pixel distance to cm. Value here should be the distance (in cm) between the inner corners of the square box.
params.downsamplerate = 20; %Downsample rate to regularise the variable framerate e.g. 10 Hz = 10
%% Process data

% Create and manipulate data in prcoessed data variable -  RawData.data_proc



% 1a. Start tracking from the first moment all body parts are confidently detected
% Anymaze videos 
% for i = 1:size(RawData, 2)
% data = RawData(i).data;
% [RawData(i).data_proc, ~] = DLC_prunestart(data, parts);
% end

% 1b. Use only the last 5 mins of the session - this is a work around for the
% time that anymaze puts the background frame at the start fo the
% video for ~5s

for i = 1:size(RawData, 2)
data = RawData(i).data;
totalframes = size(data,1);
lastfivemins = round(RawData(i).vidfps*300);
RawData(i).data_proc = data(totalframes - lastfivemins:end, :);
end


% 2. Identify x,y co-ordinates predicted outside of the the box limits and
% replace with NaNs 

% First determine the average box co-ordinates
if params.crop
    for i = 1:size(RawData, 2)
            data = RawData(i).data_proc;            
            
 for k = 1:length(boxlimits)
            crop_xy(:,k) = [median(smooth(data(:,[(boxlimits(k)-1)*3 + x + 1])));median(smooth(data(:,[(boxlimits(k)-1)*3 + y + 1])))];
        end
min_xy = [min(crop_xy(x,:)),min(crop_xy(y,:))];
max_xy = [max(crop_xy(x,:)),max(crop_xy(y,:))];

% Use cropping values to identify points outside the box and convert xy to nan
        for j = [(parts-1)*3 + x + 1]
            data(find(data(:,j) < min_xy(x)),j) = nan;
            data(find(data(:,j) > max_xy(x)),j) = nan;
        end
        for j = [(parts-1)*3 + y + 1]
            data(find(data(:,j) < min_xy(y)),j) = nan;
            data(find(data(:,j) > max_xy(y)),j)= nan;
        end
        RawData(i).data_proc = data;
    end
end

% 3. Interpolate any tracking points during session that are below criterion confidence

if params.interpolate
        for i = 1:size(RawData, 2)
            data = RawData(i).data_proc; 
            [data, percentNaNs] = DLC_interpolateLowConfidence(data,params.criterion);
            RawData(i).data_proc = data;
            RawData(i).percentNaNs = percentNaNs;
        end
end
% 4. Create xy scores for composite body parts

for i = 1:size(RawData, 2)
    data = RawData(i).data_proc; 
    [data] = appendcomposites(data, composites, composites_comp);
    RawData(i).data_proc = data;
end

% 5. Calculate pixel distance to cm 
if params.boxdimension
boxinner_x = 1 + ([innertopleft innertopright innerbottomleft innerbottomright]*3)-2;
boxinner_y = 1 + ([innertopleft innertopright innerbottomleft innerbottomright]*3)-1;
 
    for i = 1:size(RawData, 2)
        data = RawData(i).data;
        median_data = median(data, 'omitnan');
        boxcorners(:,x) = median_data(boxinner_x);
        boxcorners(:,y) = median_data(boxinner_y);
        
       RawData(i).px2cm =  mean([abs(boxcorners(1,x)- boxcorners(2,x)), 
        abs(boxcorners(3,x)- boxcorners(4,x)),
        abs(boxcorners(1,y)- boxcorners(3,y)), 
        abs(boxcorners(2,y)- boxcorners(4,y))]);
    end
end


%% 6. smooth data option
if params.smoothxy
    
    for i = 1:size(RawData, 2)
        data = RawData(i).data_proc;
        %convert time window into frame number
        smooth_window = params.smoothxy*RawData(i).vidfps;
        [data] = DLC_smoothxyparts(data, smooth_window);
        RawData(i).data_smooth = data;
    end
end 
% This takes a long time so just load it
%% Downsample data
% Also make sure there are exactly 5 mins of video data here [specific to
% this task]

for i = 1:size(RawData, 2)
    data = RawData(i).data_proc;
    fps = RawData(i).vidfps;
    downsamplerate = params.downsamplerate;
    [data] = DLC_downsample(data, fps, downsamplerate);
    
    while size(data,1) > downsamplerate*300
        data = data(2:end, :);
    end
    
    RawData(i).data_downsampled = data;
end
%%
save([folderPath+filePath+"MK801AAB_Data"],'RawData', '-v7.3');