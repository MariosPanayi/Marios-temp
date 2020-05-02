function [runlength,startindex,endindex] = consecutiveones(data)
%[startindex,endindex, runlength] = consecutiveones(data) count the number of consecutive 1s in a logical vector array
%   %Input
%   data = logical vector
%   %Output
%   startindex = start index of a run of 1s
%   endindex = end index of a run of 1s
%   runlength = lenght of a run of 1s

% Test to make sure data are (1xn) and not (nx1)
[rows, cols] =  size(data);
if rows > cols
    data = data';
end


%Calculate changes in 1s and 0s - pad with a 0 on either end
changes = diff([0 data 0]);

startindex = find(changes == 1);
endindex = find(changes == -1);
runlength = endindex - startindex;
%correct for end index beign an overesimate o
endindex = endindex - 1;

if isempty(startindex);
    startindex = 0;
    endindex = 0;
    runlength = 0;
end


end

