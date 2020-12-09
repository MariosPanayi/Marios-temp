function [smoothxy] = DLC_smoothxyparts(DLC_data, smooth_window)
%DLC_smoothxyparts() smooth xy co-ordinates for each body
%part in a DLC frame
% DLC_data = data in DLC format i.e. numeric matrix with no headers,
%   first column is frame number,
%   then groups of 3 columns for each tracked part (x,y,confidence)
%smooth_window = window of data to smooth over. Unit msut be in framenumbers. 

numparts = (size(DLC_data, 2) - 1)/3;
% numrows = size(DLC_data,1);
% First column is framenumbers again
smoothxy(:,1) = DLC_data(:,1);
for i = 1:numparts
    %identify column number for x, y, confidence for each body part (i)
    x = 1 + (i*3) - 2;
    y = 1 + (i*3) - 1;
    confidence = 1 + (i*3);
    
%Array of smoothed xy by each body part as a column
    smoothxy(:,x) =  smooth(DLC_data(:,x), smooth_window);
    smoothxy(:,y) =  smooth(DLC_data(:,y), smooth_window);
    % N.B. Confidence estimate not smoothed
    smoothxy(:,confidence) =  DLC_data(:,confidence);
end
end
