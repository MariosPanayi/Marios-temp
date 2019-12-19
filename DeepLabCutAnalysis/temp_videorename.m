% folderpath = 'E:\MK001_Videos_Converted_finalSanderson'
% directory = dir('E:\MK001_Videos_Converted_finalSanderson\MK001\Box1\1202_B_Sanderson_1');

folders = {'E:\MK001_Videos_Converted_finalSanderson\MK001\Box1',
    'E:\MK001_Videos_Converted_finalSanderson\MK001\Box2',
    'E:\MK001_Videos_Converted_finalSanderson\MK002\Box1',
    'E:\MK001_Videos_Converted_finalSanderson\MK002\Box2'};



j = 0;
for folderpath = 1:size(folders,1)
    % Find non-directory names and remove
    directory = dir(folders{folderpath});
    tf = ismember( {directory.name}, {'.', '..'});
    directory(tf) = [];
    
    for i = 1: size(directory,1)
        j = j+1;
        temp(j).folder = directory(i).folder;
        temp(j).name = directory(i).name;
    end
    
end

for i = 1:size(temp,2)
    videopath = strcat(temp(i).folder, '\', temp(i).name);
    directory = dir(videopath);
    tf = ismember( {directory.name}, {'.', '..'});
    directory(tf) = [];
    temp(i).videoname = directory.name;
    
end

%% move files to new location and rename with containing folder name

for  i = 1:size(temp,2)
oldvideopath = strcat(temp(i).folder, '\', temp(i).name,'\', temp(i).videoname);
newvideopath = strcat('E:\MK001_MK002_VideosCombinedforAnalysis\', temp(i).name,'.avi');
temp(i).newpath = newvideopath;
temp(i).newname = strcat(temp(i).name,'.avi');
%copyfile(oldvideopath, newvideopath)
end

%Save filenames for easy access later
temp1 = struct2cell(temp);
temp1 = squeeze(temp1);
xlswrite(strcat('E:\MK001_MK002_VideosCombinedforAnalysis\MK001_MK002_Filenames.xlsx'),temp1');
