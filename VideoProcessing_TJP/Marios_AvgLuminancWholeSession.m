
%read in filename
file = 'E:\SKR113_ORIENTING_VIDEOS\ACQUISITION DAY 1\BOX 1\Rat1_Record_Day_2012_09_04_Time_10_15_30_BOX1001_PAVTRAIN1_vid1.avi';
vidObj =VideoReader(file);


totalframes = vidObj.FrameRate * vidObj.Duration;
avg_luminance = zeros(totalframes,1);
for i = 1: totalframes
s.cdata = readFrame(vidObj);
avg_luminance(i) = mean(mean(rgb2gray(s.cdata)));
end