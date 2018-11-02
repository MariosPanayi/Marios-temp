%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function: pos
%
% This function allows tracking of animals by background subtracting an
% average of all frames, thresholding the remaining values and tracking the
% largest blob remaining. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function pos = position_track_video_file()

close all
%Position tracking from video
file = 'camera22018-07-25T14_09_34.mp4';
rat_thresh = 0.95;
rat_size = 50;
% Tracking objects -----------------------------
% Set blob analysis handling
hBlobRat = vision.BlobAnalysis(...
    'AreaOutputPort', true, ... 
    'CentroidOutputPort', true, ...
    'BoundingBoxOutputPort', false, ...
    'MinimumBlobArea', rat_size, ...
    'MaximumBlobArea', 3000, ...
    'MaximumCount', 10);
%-----------------------------------------------

vidObj =VideoReader(file);

vidHeight = vidObj.Height;
vidWidth = vidObj.Width;

s = struct('cdata',zeros(vidHeight,vidWidth,3,'uint8'),'colormap',[]);

%future edit: when wanting to do a whole video, use a 'while' loop - while
%there are still frames, then do the analysis...

for k = 1:500 %for the first k number of frames - input the frame data into the struct
    s(k).cdata = readFrame(vidObj);
end

% % k = 1;
% % while hasFrame(vidObj)
% %     s(k).cdata = readFrame(vidObj);
% %     k = k+1;
% % end
%calculate average signal from all frames across whole video
for k = 1:length(s)
    r(k,:,:) = s(k).cdata(:,:,1);
    g(k,:,:) = s(k).cdata(:,:,2);
    b(k,:,:) = s(k).cdata(:,:,3);
end

%average multidimensional array
mean_r = squeeze(mean(r,1));
mean_g = squeeze(mean(g,1));
mean_b = squeeze(mean(b,1));

lad_frame(:,:,1) = mean_r;
lad_frame(:,:,2) = mean_g;
lad_frame(:,:,3) = mean_b;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% background the video
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for j = 1:length(s)
    s(j).cdata = s(j).cdata-uint8(lad_frame);
end

plotfigs = 1; %turn on/off plotting
stelparam = 5;
for i = 1:length(s)
    new_frame = image(s(i).cdata);
    xy = FindPosition(new_frame.CData, rat_thresh, hBlobRat,plotfigs, stelparam);
    if ~isempty(xy)
        pos(i,:) = xy;
    else
        pos(i,:) = [0,0]; %signal loss = 0,0
    end
end

function MarkerPos = FindPosition(new_frame, rat_thresh, hBlobRat, plotfigs, stelparam)
        % Tracking steps:
        % (1) Get red/green component of the image
        % (2) Median filter
        % (3) Make binary mask
        % (4) Get centroids of top 10 points, keep biggest
        % (5) Return annotated video
            % Process red stream
            
            diffFrameRat = imcomplement(new_frame); 
            %diffFrameRat = medfilt2(diffFrameRat, [3 3]); 
                        
% %             %apply low threshold to get only light
% %             temp_thresh = 0.1;
% %             %set brightest parts of image (blackest in complement) to black 
% %             binFrameLight = im2bw(diffFrameRat, temp_thresh); 
                        
            %look for rat
            binFrameRat = im2bw(diffFrameRat, rat_thresh); 
            
% %             %subtracking pixels detected in both binFrameLight and
% %             %binFrameRat
% %             binFrameRat(~binFrameLight) = 1;
% %             %subtract light
            
%-----------------og method--------------------------------------------%
% % %             [areaRat,centroidRat] = step(hBlobRat, ~binFrameRat); 
% % %             [~,index] = max(areaRat);  
% % %             centroidRat=centroidRat(index,:);
% % %             centroidRat = uint16(centroidRat); 
%----------------------------------------------------------------------%            
           min_pixels = 50;
            [x,y] = detect_rat(binFrameRat, plotfigs, stelparam, min_pixels);
            centroidRat = [x,y];
            if ~isempty(centroidRat)
                MarkerPos = centroidRat;
                
                if plotfigs
                    figure
                    imshow(new_frame)
                    hold on 
                    plot(centroidRat(1),centroidRat(2),'go')

                    figure
                    imshow(binFrameRat)
                    hold on 
                    plot(centroidRat(1),centroidRat(2),'go')
                end
            else
                MarkerPos = [NaN,NaN];
            end
            
            
end
end