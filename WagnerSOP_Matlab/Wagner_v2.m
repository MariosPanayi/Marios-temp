
% Trial structure

%name Each type of stimulus here, provide a unique number that refers to
%the order they are represented in
Cxt = 1;
S1 = 2;
S2 = 3;
% Order of stimuli, 1 = present,0 = not present. Note that each
% presentation is a "unit" of stimulus change e.g. when only the context is
% present vs when a stimulus is present. rows = stimulus ID, cols = trial/time 
S.order = [1,1,1,1,1,1,1;
           0,1,0,1,0,0,0;
           0,0,0,0,0,1,0];
S.id = [Cxt, S1, S2];
%The duration of each column of S.order in arbitrary time slices
S.dur = [60,600,60,600,60,600,60];


% Creates a trial_structure variable in time slices where each 
trial_structure = zeros(size(S.order,1), sum(S.dur));
index = 0;
for i = 1:size(S.order,2)
     trial_structure(:, index+1:index+S.dur(i)) = repmat(S.order(:,i),1,S.dur(i));
 index = index + S.dur(i);
end


%% Set Up Nodes

%Initialize values
pI = 1;
pA1 = 0;
pA2 = 0;
% Order of states
I = 1;
A1 = 2;
A2 = 3;
states = [I,A1,A2];
num_states = size(states,2);
%list of stimuli in task
num_stimuli = size(trial_structure,1);
%Session length
total_time = size(trial_structure,2);

% Generate Nodes(Stimulus, State, Time)
nodes = zeros(num_stimuli,num_states, total_time);
%Initialize all stimuli in I for first time slice
nodes(:, I, 1) = 1;
%% Paramaters
%Probability of I->A1 when stimulus present. Proportional to stimulus salience
%Stimulus specific
%Mazur/Wagner: 0.6 = US; Short CS = 0.3; Long CS = 0.1;
% default p1 for [Cxt, S1, S2]
% p1 = [0.01;0.1;0.1];
p1 = [0.001;0.01;0.01];

%probability of decay from A1->A2 and A2->I
%pd2 = pd1/5; % Wagner Rule of Thumb [Mazur/Wagner = 0.1/0.02]
% pd1 = 0.1;
% pd2 = 0.02;
pd1 = 0.1;
pd2 = 0.02;

%C1 = Summed proportion of elements in A1 [Mazur/Wagner = 2]
%C2 = Summed proportion of elements in A2 [Mazur/Wagner = 10]
%C1<C2; when no stimuli are present, pd1/pd2 ~ C2/C1
%%%%% Later on these will be used for the following
%Decrement pA1 by [Delta]pA1/C1
%Decrement pA2 by [Delta]pA2/C2
% C1 = 2;
% C2 = 10;
C1 = 2;
C2 = 10;


% p_d1 = pd1 + [Delta]pA1,X/C1;
% p_d2 = pd2 + [Delta]pA2,X/C2;
% r1 = contribution of A1 activity in CS(s) to US retrieval into A2 
% r2 = contribution of A2 activity in CS(s) to US retrieval into A2
% [Wagner assumes that these proportions make the contribution of CS A2
% activity to be practically negligible]
% r1 = 1 ;
% r2 = 0.01;
r1 = 1 ;
r2 = 0.01;

%Separate learning rate parameters for excitatory and inhibitory learning
% L_excitatory = 0.1;
% L_inhibitory = 0.02;
L_excitatory = 0.1;
L_inhibitory = 0.02;

%N.B. general parameter setting rules for a normal simulation
%pd1>pd2; C1/C2 ~ pd2/pd1 = L+/L-; r1>r2 & r2/r1 samll enough to minimise
%second-order spread of activation
%Absolute values here are not as important as relative values

%Bush and Mosteller rule
%p2,US/CS = VCS-US(r1*pA1,CS +r2*pA2.CS), 0<= p2 <= 1,
%Rescorla-Wagner (1972) Summation rule
%p2,US/CS = SumVCS(i)-US(r1*pA1,CS(i) +r2*pA2.CS(i)), 0<= p2 <= 1,
%% Create Matrices to represent Changing states/values over time

%Learning rules
%[Delta]V+_CS-US = L+ * Sum_t(pA1,CS * pA1,US)
%[Delta]V-_CS-US = L- * Sum_t(pA1,CS * pA2,US)
%[Delta]Vtotal_CS-US = [Delta]V+_CS-US - [Delta]V-_CS-US
%L+ > L-

%Distractor Rules - extra variables
%p_d1 = pd1 + [Delta]pA1/C1 (written as p'd1 in Wagner 1981 -> See distractor rules allowing new stimuli to push old stimuli out in addition to base rate of decay)
p_d1 = zeros(num_stimuli,total_time);
p_d2 = zeros(num_stimuli,total_time);


%Initialise V+ and V- values per trial. Matrix of all combinaitons of stimuli
% set up such that direction of associations is S1->S2, in rows,cols (S1,S2)
% V_Total = V_excitatory - V_inhibitory
% N.B. Diagonal of this matrix will be the Value of a stimulus to prime
% itself
V_Excitatory = zeros(num_stimuli,num_stimuli,total_time);
V_Inhibitory = zeros(num_stimuli,num_stimuli,total_time);
V_Total = zeros(num_stimuli,num_stimuli,total_time);
DeltaV_Excitatory = zeros(num_stimuli,num_stimuli,total_time);
DeltaV_Inhibitory = zeros(num_stimuli,num_stimuli,total_time);
DeltaV_Total = zeros(num_stimuli,num_stimuli,total_time);

% Change in proprtion of elements in each state at each point in time
delta_I_A1 = zeros(num_stimuli,total_time);
delta_A1_A2 = zeros(num_stimuli,total_time);
delta_A2_I = zeros(num_stimuli,total_time);
delta_I_A2 = zeros(num_stimuli,total_time);

%% Initialise Learning loop
tic
for t = 2:total_time
   present_stimuli = trial_structure(:,t);
   
    %(1) I->A1
    %%%%%(i) Calculate elements to remove from I based on current stimuli, p1 and availability in I
    delta_I_A1 = p1.*present_stimuli.*nodes(:,I,t-1);
    %%%%%(ii) Update I
    nodes(:,I,t) = nodes(:,I,t-1) - delta_I_A1;
    %%%%%(iii) Update A1
    nodes(:,A1,t) = nodes(:,A1,t-1) + delta_I_A1;
    
    %(2) A1->A2
    %%%%%(i) Calculate elements to remove from A1 based on p1 and capacity of A1
    p_d1(:,t) = pd1 + sum(delta_I_A1)/C1; % Common decay
    %p_d1(:,t) = pd1 + delta_I_A1./C1;   % Individual decay
    delta_A1_A2 = p_d1(:,t).*nodes(:,A1,t);
    %%%%%(i) Update A1
    nodes(:,A1,t) = nodes(:,A1,t) - delta_A1_A2;
    %%%%%(ii) Update A2
    nodes(:,A2,t) = nodes(:,A2,t-1) + delta_A1_A2;
        
end
toc