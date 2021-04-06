function [trialTab,sessionTab]=SelectExperiment(trialTab,sessionTab,experimentString)
% This helper function helps to select the data from one of the experiments
% presented in "Striatal Dopamine Mediates Hallucination-Like Perception in
% Mice" by Schmack et al. 2020. KS, April 2020, Cold Spring Harbor,
% schmack@cshl.edu

p=inputParser;
validTable = @(x) istable(x) & sum(ismember(x.Properties.VariableNames,{'subjectId','sessionId'}))==2;
p.addRequired('trialTab', validTable);
p.addRequired('sessionTab', validTable);
p.addRequired('experimentString',@(x) any(validatestring(x,{'behavior','expectations','ketamine','optogenetics','optogeneticsHaloperidol'})));

p.parse(trialTab,sessionTab,experimentString)

% this helper function filters trialTab and sessionTab to include  only data from the experiment given in experimentString 
sesIdx=ismember(sessionTab.experiment,experimentString);
keyVar=unique(sessionTab(sesIdx,{'subjectId','sessionId'}));
trialIdx=ismember(trialTab(:,{'subjectId','sessionId'}),keyVar);

trialTab=trialTab(trialIdx,:);
sessionTab=sessionTab(sesIdx,:);
end