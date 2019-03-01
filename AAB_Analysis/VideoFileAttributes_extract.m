% Quick video file information extraction
cd 'F:\Marios 2017\MP003_SameDiff1_VideoConversion'
videolist = dir( '*.mp4');
for i = 1:size(videolist,1)
    obj = VideoReader(videolist(i).name);
    video(i).name = obj.Name;
    video(i).fps = obj.FrameRate;
    video(i).height = obj.Height;
    video(i).width = obj.Width;
    video(i).Duration = obj.Duration;
end

writetable(struct2table(video), 'videoattributes.csv')

%'F:\Marios 2017\MP003_SameDiff1_VideoConversion\videoattributes.csv'

