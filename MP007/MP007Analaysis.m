close all
clear all




%Load Data into cell array
rawdata = mpc_read_multiple_data('C:\Users\mpanagi\Documents\GitHub\Marios-temp\MP007\MP007_LightLeverPress_Acquisition_Day10');

% Matrix location of event and timestamp data in med associates data
eventMatrix = 'A';
tsMatrix = 'B';

%Active lever is in list L; 1 = Left, 2 = Right N.B. this is based on the inputs of the Med Box, the correspondign ts is Left lever = 5, Right lever = 7
LeverIdentityList = 'L';
leftLeverid = 5;
rightLeverid = 7;

responseBins_All = [];
%%
for i = size(rawdata,2) 
    %1:size(rawdata,2)
    
    %% Extract information first
    %Extract subject/session information
    Start_Date = rawdata{1,i}.Start_Date;
    End_Date = rawdata{1,i}.End_Date;
    Subject = rawdata{1,i}.Subject;
    Experiment = rawdata{1,i}.Experiment;
    Group = rawdata{1,i}.Group;
    Box = rawdata{1,i}.Box;
    Start_Time = rawdata{1,i}.Start_Time;
    End_Time = rawdata{1,i}.End_Time;
    MSN = rawdata{1,i}.MSN;
    
    %Establish Lever identity
    if rawdata{1,i}.(LeverIdentityList) == 1
        activeLP = leftLeverid;
        Active_Lever = 'Left';
    else
        activeLP = rightLeverid;
        Active_Lever = 'Right';
    end
    
    %Assign values of events and timestamps to temporary variables for the analysis
    event = rawdata{1,i}.(eventMatrix);
    ts = rawdata{1,i}.(tsMatrix);
        data = [event; ts]';
        
    %% Analyse data
    [ IRI, targetExtracted ] = extractRFrequency( data, activeLP );
    binSize = 60;
    session_Length = 1800;    
    [ timeBins, responseBins ] = extractRFrequency_timebins( targetExtracted, binSize, session_Length );
    
      responseBins_All(i) = responseBins;
    
end