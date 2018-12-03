%Find all the .avi files: N.B.  file number 32,216, 400 throw up errors
%when reading by VideoReader function - need to install 64 bit h.264 codecs

containingfolder = 'G:\SKR113_ORIENTING_VIDEOS\**\**\*.avi';
Filelist = dir('G:\SKR113_ORIENTING_VIDEOS\**\*.avi');
%Filelist(i).name
%Filelist(i).folder


for i = 180:size(Filelist,1)
    file = strcat(Filelist(i).folder, '\',Filelist(i).name)
    
%     vidObj = VideoReader(file);
%     totalframes = vidObj.FrameRate * vidObj.Duration;
%     avg_luminance = zeros(ceil(totalframes),1);
%     for j = 1: totalframes
%         cdata = readFrame(vidObj);
%         avg_luminance(j) = mean(mean(rgb2gray(cdata)));
%     end
    matfilename = Filelist(i).name;
    matfilename = matfilename(1:end-4);
%     save(strcat(matfilename, '.mat'),'avg_luminance')
    clear avg_luminance
end