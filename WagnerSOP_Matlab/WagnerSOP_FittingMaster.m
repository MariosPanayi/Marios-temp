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

params.p1 = [0.01;0.1;0.1];
params.pd1 = 0.1;
params.pd2 = 0.02;
params.pA2_A1 = 0;
params.C1 = 2;
params.C2 = 10;
params.r1 = 1;
params.r2 = 0.01;
params.L_excStim = 0.1; 
params.L_excSelf = 0.00;
params.L_inhStim = 0.02;
params.L_inhSelf = 0.00;
params.permissible_assoc  = [1, 1, 1;
                             1, 1, 1;
                             1, 1, 1];
                         
                         
[data] = WagnerSOP_fit(trial_structure,params);


                         
                         
% %% Paramaters explained
% %Probability of I->A1 when stimulus present. Proportional to stimulus salience
% %Stimulus specific
% %Mazur/Wagner: 0.6 = US; Short CS = 0.3; Long CS = 0.1;
% % default p1 for [Cxt, S1, S2]
% % p1 = [0.01;0.1;0.1];
% p1 = [0.01;0.1;0.1];
% 
% %probability of decay from A1->A2 and A2->I
% %pd2 = pd1/5; % Wagner Rule of Thumb [Mazur/Wagner = 0.1/0.02]
% % pd1 = 0.1;
% % pd2 = 0.02;
% pd1 = 0.1;
% pd2 = 0.02;
% 
% %C1 = Summed proportion of elements in A1 [Mazur/Wagner = 2]
% %C2 = Summed proportion of elements in A2 [Mazur/Wagner = 10]
% %To allow for the model to use values of 0 without a divide by 0 error, C1
% %and C2 are enterred as their reciprocals i.e. 1/2 instead of 2 so that
% %multiplication is used instead
% %C1<C2; when no stimuli are present, pd1/pd2 ~ C2/C1
% %%%%% Later on these will be used for the following
% %Decrement pA1 by [Delta]pA1/C1
% %Decrement pA2 by [Delta]pA2/C2
% % C1 = 2;
% % C2 = 10;
% % N.B. This is not implemented in the SOP Simulator (Byers, Mondragon, Alonso, 2017)
% C1 = 2;
% C2 = 10;
% 
% 
% % p_d1 = pd1 + [Delta]pA1,X/C1;
% % p_d2 = pd2 + [Delta]pA2,X/C2;
% % r1 = contribution of A1 activity in CS(s) to US retrieval into A2
% % r2 = contribution of A2 activity in CS(s) to US retrieval into A2
% % [Wagner assumes that these proportions make the contribution of CS A2
% % activity to be practically negligible]
% % r1 = 1 ;
% % r2 = 0.01;
% % N.B. This is not implemented in the SOP Simulator (Byers, Mondragon, Alonso, 2017)
% r1 = 1 ;
% r2 = 0.01;
% 
% %Separate learning rate parameters for excitatory and inhibitory learning
% % general Parameters
% % L_excitatory = 0.1;
% % L_inhibitory = 0.02;
% % Matrix of values for each possible Stim->Stim learning rate to be
% % specified separately if needed
% 
% % Excitatory Learning parameter for stim1->stim2
% L11 = 0.1;
% % Excitatory Learning parameter for stim1->stim1 self priming
% L12 = 0.1;
% % Inhibitory Learning parameter for stim1->stim2
% % Ll2 = L11/5;
% L21 = 0.02;
% % Inhibitory Learning parameter for stim1->stim1 self priming
% L22 = 0.02;
% % L22 = L21/5;
% 
% 
% L_excitatory = [L12, L11,L11;
%     L11, L12, L11;
%     L11, L11, L12];
% 
% L_inhibitory = [L22, L21, L21;
%     L21, L22, L21;
%     L21, L21, L22];
% 
% 
% 
% %N.B. general parameter setting rules for a normal simulation
% %pd1>pd2; C1/C2 ~ pd2/pd1 = L+/L-; r1>r2 & r2/r1 samll enough to minimise
% %second-order spread of activation
% %Absolute values here are not as important as relative values
% 
% %Bush and Mosteller rule
% %p2,US/CS = VCS-US(r1*pA1,CS +r2*pA2.CS), 0<= p2 <= 1,
% %Rescorla-Wagner (1972) Summation rule
% %p2,US/CS = SumVCS(i)-US(r1*pA1,CS(i) +r2*pA2.CS(i)), 0<= p2 <= 1,
% 
% % choose direction of learning i.e. CS-> US only or CS <-> US associations
% % possible etc....
% % This will be applied to the V_Total variable
% % Matrix of all combinaitons of stimuli
% % set up such that direction of associations is S1->S2, in rows,cols (S1,S2)
% % permissible_assoc  = [0, 1, 1;
% %                       1, 0, 1;
% %                       1, 1, 0];
% % %
% permissible_assoc  = [1, 1, 1;
%                       1, 1, 1;
%                       1, 1, 1];
% 
% 
% % Optional allowance of elements to transition from A2 back to A1
% %Set to 0 to remove this modification
% pA2_A1 = 0;