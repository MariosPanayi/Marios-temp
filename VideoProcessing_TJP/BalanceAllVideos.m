%Find all the .avi files: N.B.  file number 32,216, 400 throw up errors
%when reading by VideoReader function - need to install 64 bit h.264 codecs

% containingfolder = 'E:\SKR113_ORIENTING_VIDEOS\**\**\*.avi';
Filelist = dir('E:\SKR113_ORIENTING_VIDEOS\**\*.avi');
%Filelist(i).name
%Filelist(i).folder
savefolder = 'C:\Users\mpanagi\Desktop\OrientingVideosBalanced';
luminance = 90;

for i = 2:30
    file = strcat(Filelist(i).folder, '\',Filelist(i).name)
    vidObj = VideoReader(file);
    savefile = strcat(savefolder, '\',Filelist(i).name);
    balanced = VideoWriter(savefile);
    balanced.FrameRate = vidObj.FrameRate;
    open(balanced);
    
    endTime = vidObj.Duration;
    while vidObj.CurrentTime < endTime
        cdata = readFrame(vidObj);
        avg_lum = mean(mean(rgb2gray(cdata)));
        correction = avg_lum/luminance;
        cdata = cdata/correction;
        writeVideo(balanced,cdata);
    end
    
    close(balanced);
    
    clear balanced vidObj  
end




