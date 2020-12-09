function [ durationData, eventData ] = extractRDuration(data, startIndex, endIndex, missingData )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

        
%  Extract event start and end positions
        startData = data(find(data(:,1)==startIndex),2);
        endData = data(find(data(:,1)==endIndex),2);
%Correct for event starting before data recording, or event ending after
%data recording
        if size(startData,1) > size(endData,1)
            startData(size(startData,1)) = [];
        elseif size(startData,1) < size(endData,1)
            endData(1) = [];
        end
       
       durationData = endData-startData;
       eventData = startData; 
end

