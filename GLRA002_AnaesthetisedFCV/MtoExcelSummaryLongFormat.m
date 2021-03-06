%GLRA002 Sumamry to Excel data - Long format
%load in .m data files for GLRA002Analysis and run this script


excelData = {};
position = 0;
for i = 1:size({data.subject},2)
   trialNum = 1;
   for j = 1:size(data(i).stim_params.stimFreq,2);
   position = position+1;   
   
   excelData{position,1} = data(i).experiment; 
   excelData{position,2} = data(i).subject;
   excelData{position,3} = data(i).date;
   excelData{position,4} = char(data(i).genotype);
   excelData{position,5} = char(data(i).sex);
   excelData{position,6} = data(i).calibrationFactor;
   
   excelData{position,7} = data(i).stim_params.stimFreq(1,j);
   excelData{position,8} = data(i).stim_params.stimPulses(1,j);
   excelData{position,9} = data(i).stim_params.stimStrength(1,j);
   excelData{position,10} = data(i).summary.DA_max(1,j);
   excelData{position,11} = data(i).summary.DA_max(1,j)/data(i).calibrationFactor;
   excelData{position,12} = data(i).summary.DA_latency(1,j);
   
   excelData{position,13} = trialNum;
   trialNum = trialNum + 1;
   
   end
end

colTitles = {'experiment','subject', 'date','genotype','sex','calibrationFactor','stimFreq','StimPulses', 'StimStrength','DA_max','DA_max_cal','DA_latency', 'trialNumber'};

excelTemp = [colTitles;excelData];

xlswrite('F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_06DrugPeriodSummary.xlsx', [colTitles;excelData]);

