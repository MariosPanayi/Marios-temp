
% Trial structure

%name Each type of stimulus here, provide a unique number that refers to
%the order they are represented in
Cxt = 1;
S1 = 2;
S2 = 3;

% Order of stimuli, 1 = present,0 = not present. Note that each
% presentation is a 'unit' of stimulus change e.g. when only the context is
% present vs when a stimulus is present. rows = stimulus ID, cols = trial/time
%
% S.order =    [1,1,1,1,1,1;
%               0,1,0,1,0,0;
%               0,0,0,0,0,1];
% 
S.order =    [1,1,1,1,1,1;
              0,1,0,1,0,1;
              0,0,0,0,0,0];

% S.order =    [0,0,0,0,0,0;
%               0,1,0,1,0,0;
%               0,0,0,0,0,1];

% S.order =    [0,0,0,0,0,0;
%               0,1,0,1,0,1;
%               0,0,0,0,0,0];
% 
% 
% S.order =    [0,0,0,0,0,0;
%     1,1,1,1,1,1;
%     0,0,0,0,0,0];




% S.order =    [1,1,1;
%               0,1,1;
%               0,0,1];

S.id = [Cxt, S1, S2];
%The duration of each column of S.order in arbitrary time slices
S.dur = [60,60,12,60,12,60];

repeats = 1;
S.order = repmat(S.order,1,repeats);
S.dur = repmat(S.dur,1,repeats);

% Creates a trial_structure variable in time slices where each
trial_structure = zeros(size(S.order,1), sum(S.dur));
index = 0;
for i = 1:size(S.order,2)
    trial_structure(:, index+1:index+S.dur(i)) = repmat(S.order(:,i),1,S.dur(i));
    index = index + S.dur(i);
end


%% Paramaters
%Probability of I->A1 when stimulus present. Proportional to stimulus salience
%Stimulus specific
%Mazur/Wagner: 0.6 = US; Short CS = 0.3; Long CS = 0.1;
% default p1 for [Cxt, S1, S2]
% p1 = [0.01;0.1;0.1];
p1 = [0.01;0.1;0.1];

%probability of decay from A1->A2 and A2->I
%pd2 = pd1/5; % Wagner Rule of Thumb [Mazur/Wagner = 0.1/0.02]
% pd1 = 0.1;
% pd2 = 0.02;
pd1 = 0.1;
pd2 = 0.02;

%C1 = Summed proportion of elements in A1 [Mazur/Wagner = 2]
%C2 = Summed proportion of elements in A2 [Mazur/Wagner = 10]
%To allow for the model to use values of 0 without a divide by 0 error, C1
%and C2 are enterred as their reciprocals i.e. 1/2 instead of 2 so that
%multiplication is used instead
%C1<C2; when no stimuli are present, pd1/pd2 ~ C2/C1
%%%%% Later on these will be used for the following
%Decrement pA1 by [Delta]pA1/C1
%Decrement pA2 by [Delta]pA2/C2
% C1 = 2;
% C2 = 10;
% N.B. This is not implemented in the SOP Simulator (Byers, Mondragon, Alonso, 2017)
C1 = 1/2;
C2 = 1/10;


% p_d1 = pd1 + [Delta]pA1,X/C1;
% p_d2 = pd2 + [Delta]pA2,X/C2;
% r1 = contribution of A1 activity in CS(s) to US retrieval into A2
% r2 = contribution of A2 activity in CS(s) to US retrieval into A2
% [Wagner assumes that these proportions make the contribution of CS A2
% activity to be practically negligible]
% r1 = 1 ;
% r2 = 0.01;
% N.B. This is not implemented in the SOP Simulator (Byers, Mondragon, Alonso, 2017)
r1 = 1 ;
r2 = 0.01;

%Separate learning rate parameters for excitatory and inhibitory learning
% general Parameters
% L_excitatory = 0.1;
% L_inhibitory = 0.02;
% Matrix of values for each possible Stim->Stim learning rate to be
% specified separately if needed

% Excitatory Learning parameter for stim1->stim2
L11 = 0.1;
% Excitatory Learning parameter for stim1->stim1 self priming
L12 = 0.1;
% Inhibitory Learning parameter for stim1->stim2
% Ll2 = L11/5;
L21 = 0.02;
% Inhibitory Learning parameter for stim1->stim1 self priming
L22 = 0.02;
% L22 = L21/5;


L_excitatory = [L12, L11,L11;
    L11, L12, L11;
    L11, L11, L12];

L_inhibitory = [L22, L21, L21;
    L21, L22, L21;
    L21, L21, L22];



%N.B. general parameter setting rules for a normal simulation
%pd1>pd2; C1/C2 ~ pd2/pd1 = L+/L-; r1>r2 & r2/r1 samll enough to minimise
%second-order spread of activation
%Absolute values here are not as important as relative values

%Bush and Mosteller rule
%p2,US/CS = VCS-US(r1*pA1,CS +r2*pA2.CS), 0<= p2 <= 1,
%Rescorla-Wagner (1972) Summation rule
%p2,US/CS = SumVCS(i)-US(r1*pA1,CS(i) +r2*pA2.CS(i)), 0<= p2 <= 1,

% choose direction of learning i.e. CS-> US only or CS <-> US associations
% possible etc....
% This will be applied to the V_Total variable
% Matrix of all combinaitons of stimuli
% set up such that direction of associations is S1->S2, in rows,cols (S1,S2)
% permissible_assoc  = [0, 1, 1;
%                       1, 0, 1;
%                       1, 1, 0];
% %
permissible_assoc  = [1, 1, 1;
                      1, 1, 1;
                      1, 1, 1];


% Optional allowance of elements to transition from A2 back to A1
%Set to 0 to remove this modification
pA2_A1 = 0;
%% Set Up Nodes
% Initialize values
pI = 1;
pA1 = 0;
pA2 = 0;
% Order of states
I = 1;
A1 = 2;
A2 = 3;
states = [I,A1,A2];
num_states = size(states,2);
% list of stimuli in task
num_stimuli = size(trial_structure,1);
% Session length
total_time = size(trial_structure,2);

% Generate Nodes(Stimulus, State, Time)
nodes = zeros(num_stimuli,num_states, total_time);
% Initialize all stimuli in I for first time slice
nodes(:, I, 1) = 1;

%% Create Matrices to represent Changing states/values over time

%Learning rules
%[Delta]V+_CS-US = L+ * Sum_t(pA1,CS * pA1,US)
%[Delta]V-_CS-US = L- * Sum_t(pA1,CS * pA2,US)
%[Delta]Vtotal_CS-US = [Delta]V+_CS-US - [Delta]V-_CS-US
%L+ > L-

%Distractor Rules - extra variables
%p_d1 = pd1 + [Delta]pA1/C1 (written as p'd1 in Wagner 1981 -> See distractor rules allowing new stimuli to push old stimuli out in addition to base rate of decay)
% proportion of elements to decay from A1->A2
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

% Change in proportion of elements in each state at each point in time
delta_I_A1  = zeros(num_stimuli);
delta_A1_A2 = zeros(num_stimuli);
delta_A2_I  = zeros(num_stimuli);
delta_I_A2  = zeros(num_stimuli);
delta_A2_A1 = zeros(num_stimuli);

%% Initialise Learning loop
tic
for t = 2:total_time
    present_stimuli = trial_structure(:,t);
    
    
    
    %(4) I->A2
    %%%%% Calculate the sum V for all stimuli based on all other stimuli. N.B.
    %%%%% removal of diagonals prior to summation is to remove influence of
    %%%%% self cueing
    V_Total_learn = V_Total(:,1:num_stimuli,t-1);
    
    
    %calculate contribution of sumV Total to p2 i.e. multiply SumV by
    %proportion of elements in A1 and A2
    for i = 1:num_stimuli
        SumV_Total_p2(i,:) = (r1.*nodes(1:num_stimuli,A1,t-1).*V_Total_learn(:,i)) + (r2.*nodes(1:num_stimuli,A2,t-1).*V_Total_learn(:, i));
    end
    %sum the values for all stimuli to create p2. Ensure 0< p2 <1
    p2_S1_S2 = sum(SumV_Total_p2,2);
    p2_S1_S2(p2_S1_S2<=0) = 0;
    p2_S1_S2(p2_S1_S2>=1) = 1;
    
    %Calulate change in I->A2 based on these probabilities
    delta_I_A2 = p2_S1_S2.*present_stimuli.*nodes(1:num_stimuli,I,t-1);
    
    
    %Rescorla-Wagner (1972) Summation rule
    %p2,US/CS = SumVCS(i)-US(r1*pA1,CS(i) +r2*pA2.CS(i)), 0<= p2 <= 1,
    
    %Update I
    nodes(1:num_stimuli,I,t) = nodes(1:num_stimuli,I,t-1) - delta_I_A2;
    %Update A2
    nodes(1:num_stimuli,A2,t) = nodes(1:num_stimuli,A2,t-1) + delta_I_A2;
    %Update A1 [ no update, but for consistency of t subscript, replicate values]
    nodes(1:num_stimuli,A1,t) = nodes(1:num_stimuli,A1,t-1);
    
    
    
    %Optional allow A2 to A1 pathway
    if pA2_A1
        
        delta_A2_A1 = pA2_A1*p2_S1_S2.*present_stimuli.*nodes(1:num_stimuli,A2,t);
        
        %Update A2
        nodes(1:num_stimuli,A2,t) = nodes(1:num_stimuli,A2,t) - delta_A2_A1;
        %Update A1 [ no update, but for consistency of t subscript, replicate values]
        nodes(1:num_stimuli,A1,t) = nodes(1:num_stimuli,A1,t) + delta_A2_A1;
    end
    
    
    
    %(1) I->A1
    %%%%%(i) Calculate elements to remove from I based on current stimuli, p1 and availability in I
    delta_I_A1 = p1.*present_stimuli.*nodes(:,I,t);
    %%%%%(ii) Update I
    nodes(:,I,t) = nodes(:,I,t) - delta_I_A1;
    %%%%%(iii) Update A1
    nodes(:,A1,t) = nodes(:,A1,t) + delta_I_A1;
    
    %(2) A1->A2
    %%%%%(i) Calculate elements to remove from A1 based on p1 and capacity of A1
    p_d1(:,t) = pd1 + max((nodes(:,A1,t)-nodes(:,A1,t-1)),0)*C1; % Common decay
    %p_d1(:,t) = pd1 + delta_I_A1./C1;   % Individual decay
    delta_A1_A2 = p_d1(:,t).*nodes(:,A1,t);
    %%%%%(i) Update A1
    nodes(:,A1,t) = nodes(:,A1,t) - delta_A1_A2;
    %%%%%(ii) Update A2
    nodes(:,A2,t) = nodes(:,A2,t) + delta_A1_A2;
    
    %(3) A2->I
    %%%%%(i) Calculate elements to remove from A2 based on pd2 and capacity of A1
    % p_d2(:,t) = pd2 + (sum(delta_A1_A2)+ sum(delta_I_A2))*C2;   % Common decay
    p_d2(:,t) = pd2 + max((nodes(:,A2,t)-nodes(:,A2,t-1)),0)*C2;   % Common decay
    %p_d2(:,t) = pd2 + (delta_A1_A2(:,t) + delta_I_A2(:,t))./C2;   % Individual decay
    delta_A2_I(:,t) = p_d2(:,t).*nodes(:,A2,t);
    %%%%%(ii) Update A2
    nodes(:,A2,t) = nodes(:,A2,t) - delta_A2_I(:,t);
    %%%%%(iii) Update I
    nodes(:,I,t) = nodes(:,I,t) + delta_A2_I(:,t);
    
    
    
    
    %(5) Update associative weights:
    DeltaV_Excitatory(1:num_stimuli,1:num_stimuli,t) = L_excitatory.* ((nodes(1:num_stimuli,A1,t)*nodes(1:num_stimuli,A1,t)').*permissible_assoc);
    DeltaV_Inhibitory(1:num_stimuli,1:num_stimuli,t) = L_inhibitory.* ((nodes(1:num_stimuli,A1,t)*nodes(1:num_stimuli,A2,t)').*permissible_assoc);
    
    V_Excitatory(1:num_stimuli,1:num_stimuli,t) = V_Excitatory(1:num_stimuli,1:num_stimuli,t-1) + DeltaV_Excitatory(1:num_stimuli,1:num_stimuli,t);
    V_Inhibitory(1:num_stimuli,1:num_stimuli,t) = V_Inhibitory(1:num_stimuli,1:num_stimuli,t-1) + DeltaV_Inhibitory(1:num_stimuli,1:num_stimuli,t);
    
    V_Total(1:num_stimuli,1:num_stimuli,t) = V_Excitatory(1:num_stimuli,1:num_stimuli,t) - V_Inhibitory(1:num_stimuli,1:num_stimuli,t);
    
    
end
toc


%% Figure plotting - optional
figure

subplot(1,3,1)
plot(squeeze(nodes(Cxt,A1,:)));
title('A1')
hold on
plot(squeeze(nodes(S1,A1,:)));
plot(squeeze(nodes(S2,A1,:)));
legend('Cxt', 'A', 'B')
legend('Location','northeast')
ylim([0 1])
hold off

subplot(1,3,2)
plot(squeeze(nodes(Cxt,A2,:)));
title('A2')
hold on
plot(squeeze(nodes(S1,A2,:)));
plot(squeeze(nodes(S2,A2,:)));
legend('Cxt', 'A', 'B')
legend('Location','northeast')
ylim([0 1])
hold off

subplot(1,3,3)
plot(squeeze(nodes(Cxt,I,:)));
title('I')
hold on
plot(squeeze(nodes(S1,I,:)));
plot(squeeze(nodes(S2,I,:)));
legend('Cxt', 'A', 'B')
legend('Location','northeast')
ylim([0 1])
hold off

figure

w1 = 0.8;
w2 = 0.2;
plot(squeeze(w1*nodes(S1,A1,:) + w2*nodes(S1,A2,:)))
title('Response')
% hold on
% plot(squeeze(V_Excitatory(Cxt,S1,:)))
% plot(squeeze(V_Inhibitory(Cxt,S1,:)))
% legend('VTotal', 'VExc', 'VInh')
% hold off
%
%
% plot(squeeze(V_Total(Cxt,S1,:)))
% title('SumV')
% hold on
% plot(squeeze(V_Excitatory(Cxt,S1,:)))
% plot(squeeze(V_Inhibitory(Cxt,S1,:)))
% legend('VTotal', 'VExc', 'VInh')
% hold off

% findpeaks(squeeze(nodes(S2,A2,:)))
