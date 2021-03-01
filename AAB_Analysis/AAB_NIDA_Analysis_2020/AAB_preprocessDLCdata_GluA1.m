tic
% %%
% folderPath = "D:\DLC_AllVideos_Analysis\Summary\";
% filePath = "MK801\";
% 
% load([folderPath+filePath+"MK801AAB_Data.mat"])
folderPath = "D:\DLC_AllVideos_Analysis\Summary\";
filePath = "GluA1KO\";

load([folderPath+filePath+"GluA1AAB_Data.mat"])

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
params.criterion = 1; %Threshold criterion for tracking points to interpolate
params.smoothxy = 0; %0.25 %Option to smooth data with moving average over some period of time in seconds
params.boxdimension = 40; % Parameter used to convert pixel distance to cm. Value here should be the distance (in cm) between the inner corners of the square box.
params.downsamplerate = 10; %Downsample rate to regularise the variable framerate e.g. 10 Hz = 10
params.alignandrotate = 1; %Set the co-oridnates system so that one corner of the box is at the origin and rotate all points so that adjacent box corner also lies on the x axis.
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
%% align and rotate all data so the top of the box is aligned with the x-axis 
% (top and bottom labels are flipped from label to plot co-ordinates)
if params.alignandrotate 

    for i = 1:size(RawData, 2)
            data = RawData(i).data_proc;            
            
 for k = 1:length(boxlimits)
            RawData(i).boxlimits(:,k) = [median(smooth(data(:,[(boxlimits(k)-1)*3 + x + 1])));median(smooth(data(:,[(boxlimits(k)-1)*3 + y + 1])))];
 end
 % Calculate angle between x axis and point
 % angle in radians

 origin_point = RawData(i).boxlimits(:,1);
 alignxaxis_point = RawData(i).boxlimits(:,2);
 point_xy = [alignxaxis_point - origin_point];
 % angle in polar coordinate system i.e. angle in rad relative to positive
 % X axis
 theta = atan(point_xy(2)/point_xy(1));
 % clockwise rotation
 theta = -theta;
 % Caclulate rotation matrix - https://en.wikipedia.org/wiki/Rotation_matrix
 R_theta = [cos(theta), -sin(theta);
            sin(theta), cos(theta)] ;
     %for each body part
     for j = [parts,boxinner,boxlimits]
     x_part = (j-1)*3 + x + 1;
     y_part = (j-1)*3 + y + 1;
     temp = data(:, [x_part:y_part]);
     temp = temp';
     % Change origin
     temp = temp - origin_point;
     % Rotate
     temp = (R_theta*temp);
     % Add part data back to data matrix
     data(:, [x_part:y_part]) = temp';
     end
     RawData(i).data_proc = data; 
    end

end
%%

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
RawData(i).boxlimits = crop_xy;

% Use cropping values to identify points outside the box and convert xy to nan
        for j = [(parts-1)*3 + x + 1]
            data(find(data(:,j) < min_xy(x)),j:j+1) = nan;
            data(find(data(:,j) > max_xy(x)),j:j+1) = nan;

            
        end
        for j = [(parts-1)*3 + y + 1]
            data(find(data(:,j) < min_xy(y)),j-1:j) = nan;
            data(find(data(:,j) > max_xy(y)),j-1:j) = nan;

        end
        RawData(i).min_xy = min_xy;
        RawData(i).max_xy = max_xy;
        
        RawData(i).data_proc = data;
    end
end
%% 6. smooth data option - Moved here to sort out NaN issues
if params.smoothxy
    
    for i = 1:size(RawData, 2)
        data = RawData(i).data_proc;
        %convert time window into frame number
        smooth_window = params.smoothxy*RawData(i).vidfps;
        [data] = DLC_smoothxyparts(data, smooth_window, params.criterion);
        RawData(i).data_proc = data;
    end
end 

%%
% 3. Interpolate any tracking points during session that are below criterion confidence

if params.interpolate
        for i = 1:size(RawData, 2)
            data = RawData(i).data_proc; 
            [data, percentNaNs] = DLC_interpolateLowConfidence(data,params.criterion);
            RawData(i).data_proc = data;
            RawData(i).percentNaNs = percentNaNs;
        end
end
%%
% 4. Create xy scores for composite body parts

for i = 1:size(RawData, 2)
    data = RawData(i).data_proc; 
    [data] = appendcomposites(data, composites, composites_comp);
    RawData(i).data_proc = data;
end
%%
% 5. Calculate pixel distance to cm 
if params.boxdimension
boxinner_x = 1 + ([innertopleft innertopright innerbottomleft innerbottomright]*3)-2;
boxinner_y = 1 + ([innertopleft innertopright innerbottomleft innerbottomright]*3)-1;
 
    for i = 1:size(RawData, 2)
        data = RawData(i).data;
        median_data = median(data, 'omitnan');
        boxcorners(:,x) = median_data(boxinner_x);
        boxcorners(:,y) = median_data(boxinner_y);
        
       RawData(i).px2cm =  mean([norm([boxcorners(1,:) - boxcorners(2,:)]),
       norm([boxcorners(3,:) - boxcorners(4,:)]),
       norm([boxcorners(1,:) - boxcorners(3,:)]),
       norm([boxcorners(2,:) - boxcorners(4,:)])])/params.boxdimension;
    

    end
end



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
% save([folderPath+filePath+"MK801AAB_Data"],'RawData', '-v7.3');

bodypart = (nose*3) - 1;
time_start = 1;
time_end = 30*downsamplerate;
figure


j = 1;
% cmap = bone(time_end);
for i = 6:11 
subplot(2,3,j)
hold on
xs = RawData(i).data_downsampled(time_start:time_end,bodypart) - RawData(i).min_xy(1);
ys = RawData(i).data_downsampled(time_start:time_end,bodypart+1) - RawData(i).min_xy(2);
scatter(xs, ys, 10,'filled')
xlim([0,RawData(i).max_xy(1) - RawData(i).min_xy(1)]);
ylim([0,RawData(i).max_xy(2) - RawData(i).min_xy(2)]);

j = j+1;
 
end

figure
j = 1;
% cmap = bone(time_end);

% for i = 6:11 
    for i = 7 
subplot(2,3,j)
hold on
xs = RawData(i).data_downsampled(time_start:time_end,bodypart) - RawData(i).min_xy(1);
ys = RawData(i).data_downsampled(time_start:time_end,bodypart+1) - RawData(i).min_xy(2);
distances = sqrt(diff(xs).^2 + diff(ys).^2);
plot(1:length(distances), cumsum(distances))
j = j+1;
ylim([0,3500]) 
end

xfit = 1:length(distances);
yfit = cumsum(distances);
yfitlog = log(yfit);
%%

bodypart = (head*3) - 1;
time_start = 1;
time_end = 30*downsamplerate;
figure
j = 1;

for i =  [25:27,31:33] 
subplot(2,3,j)
xs = (RawData(i).data_downsampled(time_start:time_end,bodypart) - RawData(i).min_xy(1));
ys = (RawData(i).data_downsampled(time_start:time_end,bodypart+1) - RawData(i).min_xy(2));
numbins = 3;
Xedges = [0:(RawData(i).max_xy(1)/(numbins)):RawData(i).max_xy(1)];
Yedges = [0:(RawData(i).max_xy(2)/(numbins)):RawData(i).max_xy(2)];
histogram2(xs, ys, Xedges, Yedges, 'DisplayStyle','tile')

histcountdata(j).data = histcounts2(xs, ys, Xedges, Yedges);
xlim([0,RawData(i).max_xy(1) - RawData(i).min_xy(1)]);
ylim([0,RawData(i).max_xy(2) - RawData(i).min_xy(2)]);

j = j+1;
 
end



[corr2(histcountdata(1).data, histcountdata(2).data),
corr2(histcountdata(1).data, histcountdata(3).data),
corr2(histcountdata(2).data, histcountdata(3).data);
corr2(histcountdata(4).data, histcountdata(5).data),
corr2(histcountdata(4).data, histcountdata(6).data),
corr2(histcountdata(5).data, histcountdata(6).data)]

%% Save data to .csv for subsequent analysis and plotting R
Tablevarnames = { 'frame',
'nose_x',
'nose_y',
'nose_prob',
'head1_x',
'head1_y',
'head1_prob',
'head2_x',
'head2_y',
'head2_prob',
'lear_x',
'lear_y',
'lear_prob',
'rear_x',
'rear_y',
'rear_prob',
'mid1_x',
'mid1_y',
'mid1_prob',
'mid2_x',
'mid2_y',
'mid2_prob',
'tail_x',
'tail_y',
'tail_prob',
'innertopleft_x',
'innertopleft_y',
'innertopleft_prob',
'innertopright_x',
'innertopright_y',
'innertopright_prob',
'innerbottomleft_x',
'innerbottomleft_y',
'innerbottomleft_prob',
'innerbottomright_x',
'innerbottomright_y',
'innerbottomright_prob',
'outertopleft_x',
'outertopleft_y',
'outertopleft_prob',
'outertopright_x',
'outertopright_y',
'outertopright_prob',
'outerbottomleft_x',
'outerbottomleft_y',
'outerbottomleft_prob',
'outerbottomright_x',
'outerbottomright_y',
'outerbottomright_prob',
'head_x',
'head_y',
'head_prob',
'body_x',
'body_y',
'body_prob' };
datatable = table();
for i = 1:size(RawData,2)

% Convert data for individual subject/trial into a data table with
% appropriately named variables
temptable = array2table(RawData(i).data_downsampled, 'VariableNames', Tablevarnames);
nrows = size(RawData(i).data_downsampled,1);
temptable.Properties.VariableNames([1:size(temptable,2)]) = Tablevarnames;

temptable.experiment = repmat(RawData(i).experiment, nrows, 1);
temptable.animal = repmat(RawData(i).animal, nrows, 1);
temptable.stage = repmat(RawData(i).stage, nrows, 1);
temptable.treatment = repmat(RawData(i).treatment, nrows, 1);
temptable.DLCfilename = repmat(RawData(i).DLCfilename, nrows, 1);
temptable.px2cm = repmat(RawData(i).px2cm, nrows, 1);

temptable.framerate = repmat(params.downsamplerate, nrows, 1);

temptable.min_x = repmat(RawData(i).min_xy(1), nrows, 1);
temptable.min_y = repmat(RawData(i).min_xy(2), nrows, 1);
temptable.max_x = repmat(RawData(i).max_xy(1), nrows, 1);
temptable.max_y = repmat(RawData(i).max_xy(2), nrows, 1);

% Convert actual frame number form original video into the downsampled
% frame rate number
temptable.frame = [1:nrows]';

datatable = [datatable; temptable];

end

%%
% writetable(datatable, [folderPath+filePath+"GluA1AAB_Data_downsampled_10FPS.csv"])

toc