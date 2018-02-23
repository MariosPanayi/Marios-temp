function [ IRI, targetExtracted ] = extractRFrequency( data, target )
%extractRFrequency Extracts a single event from timestamps, and also calculates inter response intervals for these responses
%   Inputs
%       data = timestamp data to be analysed, need ot be in 2 columns, column 1 = eventID, column 2 = event time stamp.
%       target = target eventID number of interest
%   Outputs
%       IRI =   column 1 provides the session time of the first response in a pair of responses
%               column 2 provides the inter-response-interval of the target
        
        %Find target data in column 2
        data1 = data(find(data(:,1)==target),2);
        data2 = data1(2:size(data1));
        targetExtracted = data1;
        %Create inter response interval data and their session time
        IRI(:,1) = data1(1:size(data1)-1);
        IRI(:,2) = data2-data1(1:size(data1)-1);       
end

