%Cut and splice videos based on Cue start and end times. Save one luminance
%balanced version and one un-corrected
%define desired average luminance
luminance = 45;

file = 'E:\SKR113_ORIENTING_VIDEOS\ACQUISITION DAY 1\BOX 1\Rat1_Record_Day_2012_09_04_Time_10_45_30_BOX1001_PAVTRAIN1_vid2.avi' ; 
vidObj = VideoReader(file);

%start/end times
startTime = (cueStart/vidObj.FrameRate) - 10;
endTime = (cueEnd/vidObj.FrameRate) + 10;

for i = 1: size(startTime,1)
    
%create new video object

balanced = VideoWriter(sprintf('balancedVid%d.avi',i))
original = VideoWriter(sprintf('OriginalVid%d.avi',i))

%define parameters here before opening video
balanced.FrameRate = vidObj.FrameRate;
open(balanced);
original.FrameRate = vidObj.FrameRate;
open(original);
   
        
vidObj.CurrentTime = startTime(i);
while vidObj.CurrentTime < endTime(i)
cdata = readFrame(vidObj);
writeVideo(original,cdata);
avg_lum = mean(mean(rgb2gray(cdata)));
correction = avg_lum/luminance;
cdata = cdata/correction; 
writeVideo(balanced,cdata);
end

close(balanced); 
close(original);
clear balanced original
end



luminance = 90;

for i = 1: size(startTime,1)
    
%create new video object

balanced = VideoWriter(sprintf('balancedVid90%d.avi',i))


%define parameters here before opening video
balanced.FrameRate = vidObj.FrameRate;
open(balanced);
   
        
vidObj.CurrentTime = startTime(i);
while vidObj.CurrentTime < endTime(i)
cdata = readFrame(vidObj);
avg_lum = mean(mean(rgb2gray(cdata)));
correction = avg_lum/luminance;
cdata = cdata/correction; 
writeVideo(balanced,cdata);
end

close(balanced); 

clear balanced original
end

