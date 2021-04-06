% This script performs analyses and plots of the data  presented in
% "Striatal Dopamine Mediates Hallucination-Like Perception in Mice" by
% Schmack et al. 2020. Figures and statistical results are stored in the
% 'results' folder. Simulations of statistical confidence are stored in the
% 'confidenceSimulation' folder.  
%
% KS, January 2021, Cold Spring Harbor schmack@cshl.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT NOTE: Some of the analyses include stochastic simulations.
% Therefore, results can vary slightly from run to run. For exactly
% repeatable results, set the random generator in the scripts (e.g.
% 'randomSeedNumber=1') to a fixed value.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACKNOWLEDGMENT: These analyses use software created and kindly made
% available in the public domain by Ian Stevenson (beeswarm.m), Alex
% Henderson (cividis.m), Rob Campbell (sigstar.m), Jose Maria
% Garcia-Valdecasas Bernal (colorgradient.m). Thank you!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear;
close all;

promptMessage = sprintf('Do you want to Analyze Mouse Data,\nor Skip this analysis?');
button = questdlg(promptMessage, 'analyze', 'analyze', 'skip', 'analyze');
if strcmpi(button, 'analyze')
  figureMouseBehavior;
end

promptMessage = sprintf('Do you want to Analyze Expectation Data,\nor Skip this analysis?');
button = questdlg(promptMessage, 'analyze', 'analyze', 'skip', 'analyze');
if strcmpi(button, 'analyze')
  figureMouseExpectations;
end

promptMessage = sprintf('Do you want to Analyze Ketamine Data,\nor Skip this analysis?');
button = questdlg(promptMessage, 'analyze', 'analyze', 'skip', 'analyze');
if strcmpi(button, 'analyze')
  figureMouseKetamine;
end

promptMessage = sprintf('Do you want to Analyze Optogenetics Data,\nor Skip this analysis?');
button = questdlg(promptMessage, 'analyze', 'analyze', 'skip', 'analyze');
if strcmpi(button, 'analyze')
  figureMouseOptogenetics;
end

promptMessage = sprintf('Do you want to Analyze Human Data,\nor Skip this analysis?');
button = questdlg(promptMessage, 'analyze', 'analyze', 'skip', 'analyze');
if strcmpi(button, 'analyze')
  figureHumanHallucinations;
end

