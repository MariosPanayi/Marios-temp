function [trial_structure] = trialgenerator_SOP(S)
% [trial_structure] = trialgenerator_SOP(S) - Marios Panayi 22/01/2020
%
% Trial structure generator for Wagner SOP simulator. Creates an N x t
% matrix of stimuli (N) occuring over time (t) with stimulus presence or
% absence indicated by [0,1]

% input: S data struct containing the params for session

%       S.order - Order of stimuli, 1 = present,0 = not present. Note that each
%                 presentation is a 'unit' of stimulus change e.g. when only the context is
%                 present vs when a stimulus is present. rows = stimulus ID, cols = trial/time
% For example row 1 is Cxt, row 2 is stimulus A, row 3 is stimulus B
% 
% Cxt always present, trials tructire is A, A, B with an ITI
% S.order =    [1,1,1,1,1,1;
%               0,1,0,1,0,0;
%               0,0,0,0,0,1];
%
%       S.dur - The duration of each column of S.order in arbitrary time slices
%               Must match number of events in S.order
%       S.dur = [600,300,60,300,60,300];
%       
%       S.ID - cell array with Stimulus identity, not necessary for
%       function but good practice to have these labels
%       S.ID = {'Cxt', 'A', 'B'};
%       
%       S.repeats - number of repeats of the sequence specified in S.order
%       and S.dur. provides an easy way of specifying a simple task
%       structure that is repetitive
%       S.repeats = 1;
%
%   output: trial_structure = matrix of stimuli (rows) over times (cols)
%           indicating whether each stimulus is present or absent [0,1]


S.order = repmat(S.order,1,S.repeats);
S.dur = repmat(S.dur,1,S.repeats);

% Creates a trial_structure variable in time slices where each
trial_structure = zeros(size(S.order,1), sum(S.dur));
index = 0;
for i = 1:size(S.order,2)
    trial_structure(:, index+1:index+S.dur(i)) = repmat(S.order(:,i),1,S.dur(i));
    index = index + S.dur(i);
end



end