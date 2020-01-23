% Parameters to pass for optimising
% params.pd1 = x(1);
% params.pd2 = x(2);
% params.w1 = x(3);

% params.L_excStim = x(4);
% params.L_excSelf = x(5);
% params.L_inhStim = x(6);
% params.L_inhSelf = x(7);

% params.p1 = [x(8);x(9);0.1];

% params.C1 = 1/x(10);
% params.C2 = 1/x(11);

% params.pA2_A1 = x(12);

modelvars = {'pd1', 'pd2', 'w1', 'L+', 'L+ self', 'L-', 'L- self','p1 Cxt', 'p1 A', 'C1', 'C2', 'pA2_A1'}';
%Initial values to start from
% x0 = [1, 1, 0.8, 0.1, 0, 0.02, 0, 0.01, 0.1, 0.5, 0.1, 0];
 
% x0 = zeros(size(modelvars))';
% x0 = ones(size(modelvars))';
% x0 = [ones(size(modelvars))/2';
  x0 = rand(size(modelvars))';

% Get WT_AAA data
run('C:\Users\mpanagi\Documents\GitHub\Marios-temp\WagnerSOP_Matlab\ModelFitting_Dataset_GluA1.m');

tic
%Define a function to generate sums of squares betweent he model sim and
%actual data
rawdata = WT_AAA;

Fsumsquares = @(x)sum((SOP_fit_SSQ_AAA(x) - rawdata).^2);
% fminunc(Fsumsquares,x0)

A = [];
b = [];
Aeq = [];
beq = [];
lb = [0,0,0,0,0,0,0,0,0,0,0,0];
ub = [1,1,1,1,1,1,1,1,1,1,1,1];
[x, fval, ~, ~, ~, grad, hess] = fmincon(Fsumsquares,x0,A,b,Aeq,beq,lb,ub);

toc


estimates = x';

parameterfit = table(modelvars, estimates)
plot(SOP_fit_SSQ_AAA(x)); hold on; plot(rawdata); hold off

R = corr([SOP_fit_SSQ_AAA(x), rawdata])
Rsq = R.^2


