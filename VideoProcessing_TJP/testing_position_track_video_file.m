%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function: pos
%
% This function allows tracking of animals by background subtracting an
% average of all frames, thresholding the remaining values and tracking the
% largest blob remaining. 
%N.B. requires: Computer Vision System Toolbox 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Use if no background frame

clear
close all
%Position tracking from video
file = 'E:\SKR113_ORIENTING_VIDEOS\ACQUISITION DAY 1\BOX 1\Rat1_Record_Day_2012_09_04_Time_10_15_30_BOX1001_PAVTRAIN1_vid1.avi'
rat_thresh = 0.95;
rat_size = 50;
% Tracking objects -----------------------------
% Set blob analysis handling
hBlobRat = vision.BlobAnalysis(...
    'AreaOutputPort', true, ... 
    'CentroidOutputPort', true, ...
    'BoundingBoxOutputPort', false, ...
    'MinimumBlobArea', rat_size, ...
    'MaximumBlobArea', 300000, ...
    'MaximumCount', 10);
%-----------------------------------------------
min_pixels = 10;
vidObj =VideoReader(file);

vidHeight = vidObj.Height;
vidWidth = vidObj.Width;

s = struct('cdata',zeros(vidHeight,vidWidth,3,'uint8'),'colormap',[]);

%future edit: when wanting to do a whole video, use a 'while' loop - while
%there are still frames, then do the analysis...
grey = zeros(vidHeight,vidWidth);

images = {'loadingbar.jpg'};
progressbar_v2(0, 0, images)
%no_frames = 1200;
no_frames = vidObj.Duration*vidObj.FrameRate;
for k = 1:no_frames %for the first k number of frames - input the frame data into the struct
    progressbar_v2(k/no_frames, 0, images,'Loading video')
    s(k).cdata = readFrame(vidObj);
    grey = grey + double(rgb2gray(s(k).cdata));
end

% % % k = 1;
% % % while hasFrame(vidObj)
% % %     s(k).cdata = readFrame(vidObj);
% % %     k = k+1;
% % % end
% % % %calculate average signal from all frames across whole video
% % for k = 1:length(s)
% %     k;
% %     r(k,:,:) = s(k).cdata(:,:,1);
% %     g(k,:,:) = s(k).cdata(:,:,2);
% %     b(k,:,:) = s(k).cdata(:,:,3);
% % end
% % 
% % %average multidimensional array
% % 
% % mean_r = squeeze(mean(r,1));
% % mean_g = squeeze(mean(g,1));
% % mean_b = squeeze(mean(b,1));
% % 
% % lad_frame(:,:,1) = mean_r;
% % lad_frame(:,:,2) = mean_g;
% % lad_frame(:,:,3) = mean_b;

lad_frame = grey/no_frames;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% background the video
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for j = 1:length(s)
    j;
    bg(j).cdata = s(j).cdata-uint8(lad_frame);
end

%%
plotfigs = 0; %turn on/off plotting
visualiseoutput = 0;
stelparam = 5;
lastxy = [];
for i = 1:length(bg)
    new_frame = image(bg(i).cdata);
    xy = FindPosition(s(i).cdata, bg(i).cdata, rat_thresh, min_pixels,hBlobRat,plotfigs, stelparam, 0, lastxy);
    if ~isempty(xy)
        pos(i,:) = xy;
    else
        pos(i,:) = [0,0]; %signal loss = 0,0
    end
    lastxy = xy;
    %write new frame with tracking on
    if visualiseoutput
        figure;
        imshow(s(i).cdata)
        hold on
        plot(pos(i,1),pos(i,2),'ro','LineWidth',2)
        pause(0.25);
        close all
        i/no_frames

    else

%         progressbar_v2(i/no_frames, 0, images,'Tracking mouse')
    end
    
end

