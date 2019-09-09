%% Laura DLC analysis
%
load('E:\MatlabAnalysis_Laura\yourmum.mat')
path = 'E:\MatlabAnalysis_Laura\';
filename = '011_13_sal_20170517DeepCut_resnet50_GoNoGoJul24shuffle1_1030000filtered.csv';
load('E:\MatlabAnalysis_Laura\011_13_step6vsal_20170517.mat');

[dataraw, bodyparts]  = DLC_RawRead([path,filename]);
%%
% Order of labeled body part columns in raw data
nose = 1;
lear = 2; 
rear = 3;
head = 4;
middle = 5;
lleg = 6;
rleg = 7;
tail = 8;
poke = 9;
llever = 10;
rlever = 11;
lmag = 12;
rmag = 13;


% Order of sub columns from Raw data
x = 1;
y = 2;
likelihood = 3;

% Interpolating low confidence params and replace in data 
criterion = 1;
filterparts = [nose, lear, rear, head, middle, lleg, rleg, tail];

[data] = interpolateLowConfidence2(dataraw,criterion, filterparts);

%% Find error cue start and end times to synchronise med and video file
%read in filename
file = 'E:\MatlabAnalysis_Laura\011_13_sal_20170517.avi';
vidObj =VideoReader(file);

totalframes = round(vidObj.FrameRate * vidObj.Duration);
avg_luminance = zeros(totalframes,1);
for i = 1: totalframes
s.cdata = readFrame(vidObj);
avg_luminance(i) = mean(mean(rgb2gray(s.cdata)));
end


% 35 and 36 are chosen based on the number of errors and the fact that the
% light goes off at the start of the session
% the  'MinPeakProminence', 7 is chosen as an arbitrary cut off - choose a
% threshold based on the max of the signal (e.g. at least 90%)
[~, error_start] = findpeaks(avg_luminance-mean(avg_luminance), 'MinPeakProminence', 7,'NPeaks', 35);

[~, error_end] = findpeaks((avg_luminance-mean(avg_luminance))*-1, 'MinPeakProminence', 5,'NPeaks', 36);

starttime = error_end(1);
error_end = error_end(2:end);

%% list of error times from MED
load('E:\MatlabAnalysis_Laura\enderror.mat');

startframe = ((error_start(1)/vidObj.FrameRate)-(time_of_end_of_error(1)/100))*vidObj.FrameRate;
startframe_time = ((error_start(1)/vidObj.FrameRate)-(time_of_end_of_error(1)/100));

% Sanity check to compare the error times between Med and the video and
% plot as a histogram
% hist((((time_of_end_of_error)-time_of_end_of_error(1))/100)'-((error_start-error_start(1))/25))



%% Convert Raw video "Frame" numbers into actual time stamps
% https://uk.mathworks.com/matlabcentral/fileexchange/61235-video-frame-time-stamps
% Use the ffmpeg conversion function from this link
% requires you to download ffmpeg.exe as well [google it]

videofile = 'E:\MatlabAnalysis_Laura\011_13_sal_20170517.avi';
%Path to the ffmpeg.exe file
ffmpeg_path = 'C:\Users\mpanagi\Downloads\ffmpeg-20190805-5ac28e9-win64-static\ffmpeg-20190805-5ac28e9-win64-static\bin';
% 
ts = videoframets(ffmpeg_path,videofile)



%% List of trial times to chunk analysis aorund: trialstartsMED
load('E:\MatlabAnalysis_Laura\video_tracking_variables.mat')

videostarttime = ts(find(abs(ts-startframe_time) == min(abs(ts-startframe_time))));
trialstartsMED = variable.trials.startimes;

% convert trial start times from Med timing to frame number on video
% Med resolution is in .01 seconds
trialstartsMED_vid = trialstartsMED+ videostarttime;

% Convert values to whole numbers, round numbers down
% period of time to analyse (in seconds)
period = 6;
cut_start = trialstartsMED_vid;
cut_end = trialstartsMED_vid + period;
%Convert time into DLC "frame number"
for i = 1: length(cut_start)
[ ~, cut_start(i)] = min(abs((ts - cut_start(i))));
[ ~, cut_end(i)] = min(abs((ts - cut_end(i))));
end

% Cut tracking data into trials
for i = 1:length(cut_start)
variable.trials.tracking{i} = data(cut_start(i):cut_end(i), :);
end


% Turn trial type variables into logicals to make indexing for extracting
% trial types and plotting more efficient/readable
Go = [];
NoGo = [];
left = [];
right = [];
single = [];
double = [];
success = [];

for i = 1:length(cut_start)
Go(i) = strcmp(variable.trials.stats{i,2}, 'GO Left') | strcmp(variable.trials.stats{i,2}, 'GO Right');
NoGo(i) = strcmp(variable.trials.stats{i,2}, 'NO GO SINGLE') | strcmp(variable.trials.stats{i,2}, 'NO GO DOUBLE');
left(i) = strcmp(variable.trials.stats{i,2}, 'GO Left');
right(i) = strcmp(variable.trials.stats{i,2}, 'GO Right');
single(i) = strcmp(variable.trials.stats{i,2}, 'NO GO SINGLE');
double(i) = strcmp(variable.trials.stats{i,2}, 'NO GO DOUBLE');

success(i) = strcmp(variable.trials.stats{i,4}, 'success');
end


% Plot Dat Shit!

%Choose body part to plot :)
plotpart = middle;

figure
set(gca,'Ydir','reverse')
set(gca,'Xdir','reverse')
hold on
for i = 1:length(cut_start)
    plot(variable.trials.tracking{1,i}(:, (plotpart-1)*3+(x+1)),variable.trials.tracking{1,i}(:, (plotpart-1)*3+(y+1)))
end
xlim([50,350])
ylim([0,250])
hold off

figure

subplot(2,2,1)
set(gca,'Ydir','reverse')
set(gca,'Xdir','reverse')
hold on
for i = find(Go & right & success)
plot(variable.trials.tracking{1,i}(:, (plotpart-1)*3+(x+1)),variable.trials.tracking{1,i}(:, (plotpart-1)*3+(y+1)))
end
xlim([50,350])
ylim([0,250])
title('Go- Right - Successful trials')
hold off

subplot(2,2,2)
set(gca,'Ydir','reverse')
set(gca,'Xdir','reverse')
hold on
for i = find(Go & left & success)
plot(variable.trials.tracking{1,i}(:, (plotpart-1)*3+(x+1)),variable.trials.tracking{1,i}(:, (plotpart-1)*3+(y+1)))
end
xlim([50,350])
ylim([0,250])
title('Go- Left - Successful trials')
hold off


subplot(2,2,3)
set(gca,'Ydir','reverse')
set(gca,'Xdir','reverse')
hold on
for i = find(NoGo & single & success)
plot(variable.trials.tracking{1,i}(:, (plotpart-1)*3+(x+1)),variable.trials.tracking{1,i}(:, (plotpart-1)*3+(y+1)))
end
xlim([50,350])
ylim([0,250])
title('NoGo- Single - Successful trials')
hold off

subplot(2,2,4)
set(gca,'Ydir','reverse')
set(gca,'Xdir','reverse')
hold on
for i = find(NoGo & double & success)
plot(variable.trials.tracking{1,i}(:, (plotpart-1)*3+(x+1)),variable.trials.tracking{1,i}(:, (plotpart-1)*3+(y+1)))
end
xlim([50,350])
ylim([0,250])
title('NoGo- Double - Successful trials')
hold off