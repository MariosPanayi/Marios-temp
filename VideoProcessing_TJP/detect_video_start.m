close all
clear
%Position tracking from video
file = 'example.mp4';

%Read video file in
vidObj =VideoReader(file);

vidHeight = vidObj.Height;
vidWidth = vidObj.Width;

s = struct('cdata',zeros(vidHeight,vidWidth,3,'uint8'),'colormap',[]);

%future edit: when wanting to do a whole video, use a 'while' loop - while
%there are still frames, then do the analysis...

timewindow = 5; %number of minutes averaged at beginning
maxframe = 25*60*timewindow;

for k = 1:maxframe %for the first k number of frames - input the frame data into the struct
    s(k).cdata = readFrame(vidObj);

    frame = rgb2gray(s(k).cdata); %convert to greyscale
    reference_rectangle = frame(60:120,1:40); %chosen rectangle at left side of box as reference
    brightness(k) = mean(reference_rectangle(:));
    
end

threshold = 3; %divide step values by this i.e. high number = small threshold

ts = [0:0.04:(0.04*maxframe)-0.04];
plot(ts,brightness)
[starttime, endtime,startindex, endindex] = get_trial_TTL(brightness,ts,threshold);

hold on
plot(ts(startindex),brightness(startindex),'go')
plot(ts(endindex),brightness(endindex),'ro')

%load in event times

filename = '/Users/LauraGrima/Dropbox/MATLAB/D1_antag_new/011_25_step6vsal_20170217';
data = mpc_read_data(filename);

%get times for light off
idx = find(data.E == 503)/100;
firstfive_event = idx(idx<300);

%match light dips ts with event ts

%look at interval differences, is it the same?

%use light ts to event ts shift to work out session start time