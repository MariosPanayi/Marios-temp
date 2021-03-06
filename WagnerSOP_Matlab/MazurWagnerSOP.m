%Things left to do
%TrialStructure generation function
%Changing value update to being trial based
%Plotting data
%AESOP
%Dickinson and Burke A2-A2 associations



%%
clear all
close all
clc

%%
%trial Structure
Total_trials = 3;
CSperTrial = 1;
CS_length = 5000;
US_Length = 0;
ISI_length = 600;

trialLength = CSperTrial*(ISI_length+CS_length);
totalTime = Total_trials*trialLength + ISI_length+ trialLength;

trialStructure = zeros(4,totalTime);
trialStructure(1,:) = 0;

for i = 0:Total_trials-1
    CS1_start = (i*trialLength)+ ISI_length;
    CS2_start = (i*trialLength)+ 2*ISI_length + CS_length;
    CS1_end = (i*trialLength)+ ISI_length+CS_length;
    CS2_end = (i*trialLength)+ 2*ISI_length + 2*CS_length;
    
    trialStructure(2,CS1_start:CS1_end) = 1;
    trialStructure(3,CS2_start:CS2_end) = 1;
    trialStructure(4,CS1_end) = 0;
    trialStructure(4,CS2_end) = 0;
    
end
%     CS1_start = (Total_trials*trialLength)+ ISI_length;
%     CS2_start = (Total_trials*trialLength)+ 2*ISI_length + CS_length;
%     CS1_end = (Total_trials*trialLength)+ ISI_length+CS_length;
%     CS2_end = (Total_trials*trialLength)+ 2*ISI_length + 2*CS_length;
%     
%     trialStructure(2,CS1_start:CS1_end) = 1;
%     trialStructure(3,CS1_start:CS1_end) = 1;
%     trialStructure(2,CS2_start:CS2_end) = 1;
%     trialStructure(3,CS2_start:CS2_end) = 1;
%     trialStructure(4,CS1_end) = 1;
%     trialStructure(4,CS2_end) = 1;




%%
%Initialize values
pI = 1;
pA1 = 0;
pA2 = 0;

I = 1;
A1 = 2;
A2 = 3;

%list of stimuli in task
Cxt = 1;
CS1 = 2;
CS2 = 3;
US = 4;

stimuli = [Cxt, CS1, CS2, US];
num_stimuli = size(stimuli,2);

%set up nodes with a row for each stimulus and a column for each state
nodes = zeros(num_stimuli,3, totalTime);
nodes(:,1) = 1;

%Probability of I->A1 when stimulus present. Proportional to stimulus salience
%Mazur/Wagner: 0.6 = US; Short CS = 0.3; Long CS = 0.1;
% p1 = [0.01;0.1;0.1;0.6];
p1 = [0.01;0.1;0.1;0.6];
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
%pd1>pd2; C1/C2 ~ pd2/pd1 = L+/L-; r1>r2 & r2/r1 samll enought o minimise
%second-order spread of activation
%Absolute values here are not as important as relative values

%Bush and Mosteller rule
%p2,US/CS = VCS-US(r1*pA1,CS +r2*pA2.CS), 0<= p2 <= 1,
%Rescorla-Wagner (1972) Summation rule
%p2,US/CS = SumVCS(i)-US(r1*pA1,CS(i) +r2*pA2.CS(i)), 0<= p2 <= 1,

%Initialise V+ and V- values per trial. Matrix of all combinaitons of stimuli
% set up such that direction of associations is S1->S2, in rows,cols (S1,S2)
%V_Total = V_excitatory - V_inhibitory
V_Excitatory = zeros(num_stimuli, num_stimuli, totalTime);
V_Inhibitory = zeros(num_stimuli, num_stimuli, totalTime);

%Learning rules
%[Delta]V+_CS-US = L+ * Sum_t(pA1,CS * pA1,US)
%[Delta]V-_CS-US = L- * Sum_t(pA1,CS * pA2,US)
%[Delta]Vtotal_CS-US = [Delta]V+_CS-US - [Delta]V-_CS-US
%L+ > L-

%%
%Initialise variables before loop
p_d1 = zeros(num_stimuli,totalTime);
delta_A1_A2 = zeros(num_stimuli,totalTime);
p_d2 = zeros(num_stimuli,totalTime);
delta_A2_I = zeros(num_stimuli,totalTime);
delta_I_A2 = zeros(num_stimuli,totalTime);
V_Excitatory = zeros(num_stimuli,num_stimuli,totalTime);
V_Inhibitory = zeros(num_stimuli,num_stimuli,totalTime);
V_Total = zeros(num_stimuli,num_stimuli,totalTime);
DeltaV_Excitatory = zeros(num_stimuli,num_stimuli,totalTime);
DeltaV_Inhibitory = zeros(num_stimuli,num_stimuli,totalTime);
V_Total = zeros(num_stimuli,num_stimuli,totalTime);

%Learning loop
for t = 2:totalTime
    currentStimuli = trialStructure(1:num_stimuli,t);
    currentI = nodes(1:num_stimuli,I,t);
    currentA1 = nodes(1:num_stimuli,A1,t);
    currentA2 = nodes(1:num_stimuli,A2,t);
    
    %(1) I->A1
    %%%%%(i) Calculate elements to remove from I based on current stimuli, p1 and availability in I
    delta_I_A1 = p1.*trialStructure(1:num_stimuli,t).*nodes(1:num_stimuli,I,t-1);
    %%%%%(ii) Update I
    nodes(1:num_stimuli,I,t) = nodes(1:num_stimuli,I,t-1) - delta_I_A1;
    %%%%%(iii) Update A1
    nodes(1:num_stimuli,A1,t) = nodes(1:num_stimuli,A1,t-1) + delta_I_A1;
    
    %(2) A1->A2
    %%%%%(i) Calculate elements to remove from A1 based on p1 and capacity of A1
    p_d1(1:num_stimuli,t) = pd1 + sum(delta_I_A1)/C1; % Common decay
    %p_d1(1:num_stimuli,t) = pd1 + delta_I_A1./C1;   % Individual decay
    delta_A1_A2(1:num_stimuli,t) = p_d1(1:num_stimuli,t).*nodes(1:num_stimuli,A1,t);
    %%%%%(i) Update A1
    nodes(1:num_stimuli,A1,t) = nodes(1:num_stimuli,A1,t) - delta_A1_A2(1:num_stimuli,t);
    %%%%%(ii) Update A2
    nodes(1:num_stimuli,A2,t) = nodes(1:num_stimuli,A2,t-1 ) + delta_A1_A2(1:num_stimuli,t);
    
    %(3) I->A2
    %%%%% Calculate the sum V for all stimuli based on all other stimuli. N.B.
    %%%%% removal of diagonals prior to summation is to remove influence of
    %%%%% self cueing
    V_Total_noDiag = V_Total(:,1:num_stimuli,t-1);
    V_Total_noDiag(1:num_stimuli+1:end) = 0;
    
    %calculate contribution of sumV Total to p2 i.e. multiply SumV by
    %porportion of elements in A1 and A2
    for i = 1:num_stimuli
        SumV_Total_p2(:,i) = (r1.*nodes(1:num_stimuli,A1,t).*V_Total_noDiag(:,i)) + (r2.*nodes(1:num_stimuli,A2,t).*V_Total_noDiag(:,i)); 
    end
    %sum the values for all stimuli to create p2. Ensure 0< p2 <1
    p2_S1_S2 = sum(SumV_Total_p2,1)';
    p2_S1_S2(p2_S1_S2<=0) = 0;
    p2_S1_S2(p2_S1_S2>=1) = 1;
    
    %Calulate change in I->A2 based on these probabilities
    delta_I_A2(1:num_stimuli,t) = p2_S1_S2.*nodes(1:num_stimuli,I,t);
        
    
    %Rescorla-Wagner (1972) Summation rule
    %p2,US/CS = SumVCS(i)-US(r1*pA1,CS(i) +r2*pA2.CS(i)), 0<= p2 <= 1,
    
    %Update I
    nodes(1:num_stimuli,I,t) = nodes(1:num_stimuli,I,t) - delta_I_A2(1:num_stimuli,t);
    %Update A2
    nodes(1:num_stimuli,A2,t) = nodes(1:num_stimuli,A2,t) + delta_I_A2(1:num_stimuli,t);
    
    
    %(4) A2->I
    %%%%%(i) Calculate elements to remove from A2 based on pd2 and capacity of A1
    p_d2(1:num_stimuli,t) = pd2 + sum(delta_A1_A2(1:num_stimuli,t)) + sum(delta_I_A2(1:num_stimuli,t))/C2;   % Common decay
%     p_d2(1:num_stimuli,t) = pd2 + (delta_A1_A2(1:num_stimuli,t) + delta_I_A2(1:num_stimuli,t))./C2;   % Individual decay
    delta_A2_I(1:num_stimuli,t) = p_d2(1:num_stimuli,t).*nodes(1:num_stimuli,A2,t);
    %%%%%(ii) Update A2
    nodes(1:num_stimuli,A2,t) = nodes(1:num_stimuli,A2,t) - delta_A2_I(1:num_stimuli,t);
    %%%%%(iii) Update I
    nodes(1:num_stimuli,I,t) = nodes(1:num_stimuli,I,t) + delta_A2_I(1:num_stimuli,t);
    
    
    %(5) Update associative weights:
    DeltaV_Excitatory(1:num_stimuli,1:num_stimuli,t) = L_excitatory * nodes(1:num_stimuli,A1,t)*nodes(1:num_stimuli,A1,t)';
    DeltaV_Inhibitory(1:num_stimuli,1:num_stimuli,t) = L_inhibitory * nodes(1:num_stimuli,A1,t)*nodes(1:num_stimuli,A2,t)';
    
    V_Excitatory(1:num_stimuli,1:num_stimuli,t) = V_Excitatory(1:num_stimuli,1:num_stimuli,t-1) + DeltaV_Excitatory(1:num_stimuli,1:num_stimuli,t);
    V_Inhibitory(1:num_stimuli,1:num_stimuli,t) = V_Inhibitory(1:num_stimuli,1:num_stimuli,t-1) + DeltaV_Inhibitory(1:num_stimuli,1:num_stimuli,t);
    
    V_Total(1:num_stimuli,1:num_stimuli,t) = V_Excitatory(1:num_stimuli,1:num_stimuli,t) - V_Inhibitory(1:num_stimuli,1:num_stimuli,t);
    
end

%%
%test that all elements sum to 1 across activation states
%delete
totalElements_Cxt(1:totalTime) = sum(nodes(Cxt,I:A2,:));
totalElements_CS1(1:totalTime) = sum(nodes(CS1,I:A2,:));
totalElements_CS2(1:totalTime) = sum(nodes(CS2,I:A2,:));
totalElements_US(1:totalTime) = sum(nodes(US,I:A2,:));

if sum(totalElements_Cxt) == totalTime | sum(totalElements_CS1)== totalTime| sum(totalElements_CS2)== totalTime| sum(totalElements_US)== totalTime
    'All proportions add up'
else
    'error = proportions do not add to 1'
end
%%



nodeActivity = figure;

subplot(3,2,1)
Cxt_A1(1:totalTime) = nodes(Cxt,A1,1:end);
CS1_A1(1:totalTime) = nodes(CS1,A1,1:end);
CS2_A1(1:totalTime) = nodes(CS2,A1,1:end);
US_A1(1:totalTime) = nodes(US,A1,1:end);

%figure_A1 = figure;
plot(Cxt_A1)
hold on
plot(CS1_A1)
plot(CS2_A1)
plot(US_A1)
title('Proportion of elements in A1')
hold off
legend('Cxt', 'CS1', 'CS2', 'US');

subplot(3,2,3)
Cxt_A2(1:totalTime) = nodes(Cxt,A2,1:end);
CS1_A2(1:totalTime) = nodes(CS1,A2,1:end);
CS2_A2(1:totalTime) = nodes(CS2,A2,1:end);
US_A2(1:totalTime) = nodes(US,A2,1:end);
%figure_A2 = figure;
plot(Cxt_A2)
hold on
plot(CS1_A2)
plot(CS2_A2)
plot(US_A2)
title('Proportion of elements in A2')
hold off

subplot(3,2,5)
Cxt_I(1:totalTime) = nodes(Cxt,I,1:end);
CS1_I(1:totalTime) = nodes(CS1,I,1:end);
CS2_I(1:totalTime) = nodes(CS2,I,1:end);
US_I(1:totalTime) = nodes(US,I,1:end);
%figure_I = figure;
plot(Cxt_I)
hold on
plot(CS1_I)
plot(CS2_I)
plot(US_I)
title('Proportion of elements in I')
hold off

%%
V_Total_Cxt_US(1:totalTime)  = V_Total(Cxt,US,1:end);
V_Total_CS1_US(1:totalTime)  = V_Total(CS1,US,1:end);
V_Total_CS2_US(1:totalTime)  = V_Total(CS2,US,1:end);
V_Excitatory_Cxt_US(1:totalTime)  = V_Excitatory(Cxt,US,1:end);
V_Excitatory_CS1_US(1:totalTime)  = V_Excitatory(CS1,US,1:end);
V_Excitatory_CS2_US(1:totalTime)  = V_Excitatory(CS2,US,1:end);
V_Inhibitory_Cxt_US(1:totalTime)  = V_Inhibitory(Cxt,US,1:end);
V_Inhibitory_CS1_US(1:totalTime)  = V_Inhibitory(CS1,US,1:end);
V_Inhibitory_CS2_US(1:totalTime)  = V_Inhibitory(CS2,US,1:end);

% SumV_US_Plot = figure;
subplot(3,2,2)
plot(V_Total_Cxt_US);
hold on
plot(V_Total_CS1_US);
plot(V_Total_CS2_US);
title('Sum V -> US');
% legend('Cxt', 'CS1', 'CS2');
hold off

subplot(3,2,4)
plot(V_Excitatory_Cxt_US);
hold on
plot(V_Excitatory_CS1_US);
plot(V_Excitatory_CS2_US);
title('V excitatory -> US');
hold off

subplot(3,2,6)
plot(V_Inhibitory_Cxt_US);
hold on
plot(V_Inhibitory_CS1_US);
plot(V_Inhibitory_CS2_US);
title('V inhibitory -> US');
hold off

% %Response generation
% Responding_plot = figure;
% w1 = 0.8;
% w2 = 0.2;
% Responding_US = w1*US_A1 + w2*US_A2;
% plot(Responding_US);
% %R = f(w1 * pA1,US + w2*pA2,US)

Prism(1:totalTime,1) = CS1_A1;
Prism(1:totalTime,2) = CS2_A1;
Prism(1:totalTime,3) = CS1_A2;
Prism(1:totalTime,4) = CS2_A2;
Prism(1:totalTime,5) = CS1_I;
Prism(1:totalTime,6) = CS2_I;
