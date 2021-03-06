%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function: pos
%
% This function allows tracking of animals by background subtracting an
% average of all frames, thresholding the remaining values and tracking the
% largest blob remaining. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Use if you have a background frame (this is much faster)

clear
close all

bg_frame = 420; %farooq - arena
bg_frame = 320; %gangrel - arena
bg_frame = 1330; %corleone - arena
led_on = 1;
%bg_frame = 1;
bg_frame_time = 455; %bossman - arena A
bg_frame_time = 673; %bossman - arena B
bg_frame_time = 1; %farooq - arena A
%bg_frame_time = 424; %faqrooq - arena B
bg_frame_time = 375; %eddie- arena A [175 bg 1, 375 other bg]
%bg_frame_time = 547; %gangrel - arena A [9min 47s 547]
%bg_frame_time = 521; %house- arena B 64[8min 41s 521]


%Position tracking from video
file = 'C:\Users\mpanagi\Documents\GitHub\newvideo1.avi';
%file = 'example.mp4';

rat_thresh = 0.95; 
rat_min_size = 500;
rat_max_size = 10000000000000;
% Tracking objects -----------------------------
% Set blob analysis handling
hBlobRat = vision.BlobAnalysis(...
    'AreaOutputPort', true, ... 
    'CentroidOutputPort', true, ...
    'BoundingBoxOutputPort', false, ...
    'MinimumBlobArea', rat_min_size, ...
    'MaximumBlobArea', rat_max_size, ...
    'MaximumCount', 10);
%-----------------------------------------------
minpixels = 10;
plotfigs = 0; %turn on/off plotting
stelparam = 5;
visualiseoutput = 0; %show frame and tracking

vidObj =VideoReader(file);

vidHeight = vidObj.Height;
vidWidth = vidObj.Width;

s = struct('cdata',zeros(vidHeight,vidWidth,3,'uint8'),'colormap',[]);

%future edit: when wanting to do a whole video, use a 'while' loop - while
%there are still frames, then do the analysis...
grey = zeros(vidHeight,vidWidth);

images = {'loadingbar.jpg','loadingbar2.jpg','loadingbar3.jpg','loadingbar4.jpg','progressbar1.jpg','progressbar2.jpg','progressbar3.jpg','progressbar4.jpg','progressbar5.jpg','progressbar6.jpg'};
progressbar_v2(0, 0, images)

no_frames = vidObj.Duration*vidObj.FrameRate;
k = 1;

%calculate average signal from all frames across a section of video
% 

%if using BG
% bg_frame = bg_frame_time*vidObj.FrameRate;
% maxframe = bg_frame;
bg_frame = [];
maxframe = no_frames;


if isempty(bg_frame)
    startFrame = 1;
    for k = startFrame:maxframe %for the first k number of frames - input the frame data into the struct
        progressbar_v2(k/maxframe, 0, images,'Loading video')
        s.cdata = readFrame(vidObj);
        grey = grey + double(rgb2gray(s.cdata));
    end
else
    for k = 1:maxframe %for the first k number of frames - input the frame data into the struct
        progressbar_v2(k/maxframe, 0, images,'Loading background frame')
        s.cdata = readFrame(vidObj);
        grey = double(rgb2gray(s.cdata));
    end
end
if isempty(bg_frame)
   avg_frame = grey/(maxframe-startFrame);
elseif bg_frame == 1
    avg_frame = zeros(vidHeight, vidWidth);
else
   avg_frame = rgb2gray(s.cdata);
end

clear s
vidObj =VideoReader(file);
k = 0;
progressbar_v2(0, 0, images)
while hasFrame(vidObj)
    s.cdata = readFrame(vidObj);
    k = k+1;
       
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % background the frame
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    bg.cdata = rgb2gray(s.cdata)-uint8(avg_frame);    

    lastxy = [];

    if k >= startFrame
        xy = FindPosition(s.cdata, bg.cdata, rat_thresh, minpixels, hBlobRat,plotfigs, stelparam, led_on, lastxy);
    else
        xy = [];
    end
    if ~isempty(xy)
        pos(k,:) = xy;
    else
        pos(k,:) = [0,0]; %signal loss = 0,0
    end
    lastxy = xy;
    %write new frame with tracking on
    if visualiseoutput
        figure;
        imshow(s.cdata)
        hold on
        plot(pos(k,1),pos(k,2),'ro','LineWidth',2)
        pause(0.25);
        close all
        k/no_frames

    else

       % progressbar_v2(k/no_frames, 0, images,'Tracking mouse')
    end
    
end
save('C:\Users\mpanagi\Documents\GitHub\Marios-temp\VideoProcessing_TJP\positiondatasample', 'pos', 'bg_frame','avg_frame')
