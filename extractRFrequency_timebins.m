function [ timeBins, responseBins ] = extractRFrequency_timebins( targetExtracted, binSize, session_Length )
%extractRFrequency_timebins takes a column of timestamps and bins them according 
%   Inputs
%       targetExtracted = column of timestamps to be binned
%       binSize = size of time bin in units of timestamp
%       session_Length = max session length in units of time stamp
%   Outputs
%       timeBins = time bin number in a vector
%       responseBins = number of responses in time bin
        
    data = targetExtracted;
    
    for i = 1: session_Length/binSize
        timeBins(i) = i;
        responseBins(i) = length(targetExtracted(targetExtracted > binSize*(i-1) & targetExtracted <binSize*(i)));
       
end

