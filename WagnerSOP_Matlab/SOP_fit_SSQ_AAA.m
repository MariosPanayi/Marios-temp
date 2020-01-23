function [modelresponse] = SOP_fit_SSQ_AAA(x)
%% Specify trial structure for model
S.ID = {'Cxt', 'A', 'B'};
%Trial Structure - AAA with Cxt always present
S.order =    [1,1,1,1,1,1;
              0,1,0,1,0,1;
              0,0,0,0,0,0];
%The duration (arbitrary timeslices - chosen to represent 5s in AAB task)
%of events in S.order. Extra long initial iti is specified to allow
%background context to saturate the model (optional).
S.dur = [60,60,12,60,12,60];
%Number of repeats of the trial structure specified in S.order
S.repeats = 1;
%generate trial_structure using function trialgenerator_SOP.m
[trial_structure] = trialgenerator_SOP(S);

%% params for SOP simulation

% params.p1 = [0.01;0.1;0.1];
params.p1 = [x(8);x(9);0.1];

% params.pd1 = 0.1;
% params.pd2 = 0.02;
params.pd1 = x(1);
params.pd2 = x(2);
% params.pA2_A1 = 0;
params.pA2_A1 = x(12);
% params.C1 = 2;
% params.C2 = 10;
params.C1 = 1/x(10);
params.C2 = 1/x(11);
params.r1 = 1;
params.r2 = 0.01;
% params.L_excStim = 0.1; 
% params.L_excSelf = 0.00;
% params.L_inhStim = 0.02;
% params.L_inhSelf = 0.00;
params.L_excStim = x(4);
params.L_excSelf = x(5);
params.L_inhStim = x(6);
params.L_inhSelf = x(7);

params.permissible_assoc  = [1, 1, 1;
                             1, 1, 1;
                             1, 1, 1];
% params.w1 = 0.8;
% params.w2 = 0.2;
params.w1 = x(3);
params.w2 = 1-params.w1;
                      
                         
 % run simulation with these parameters                        
[data] = WagnerSOP_fit(trial_structure,params);

% extact data about A1/A2 activity to combine into a composite "response"
% extract periods related to trial 1,2,3 and remove ITI

I = 1;
A1 = 2;
A2 = 3;

Atrial = find(trial_structure(2,:)==1);
Btrial = find(trial_structure(3,:)==1);
if isempty(Btrial)
A1_activity = [data.nodes(2,A1,Atrial)];
A2_activity = [data.nodes(2,A2,Atrial)];    
else
A1_activity = [data.nodes(2,A1,Atrial);data.nodes(3,A1,Btrial)];
A2_activity = [data.nodes(2,A2,Atrial);data.nodes(3,A2,Btrial)];
end

A1_activity = squeeze(A1_activity);
A2_activity = squeeze(A2_activity);

% Proportion of response driven by A1 and A2 activity is w1 and w2
% respoectively


w1 = params.w1;
w2 = params.w2;

modelresponse = [w1*A1_activity + w2*A2_activity];




end
