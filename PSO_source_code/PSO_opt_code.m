clc;
clear;

nVar=1;            % Number of Decision Variables
VarSize=[2 nVar];   % Size of Decision Variables Matrix
VarMin=-10;         % Lower Bound of Variables
VarMax= 10;         % Upper Bound of Variables
%% PSO Parameters
MaxIt=100;      % Maximum Number of Iterations
nPop=10;        % Population Size (Swarm Size)
% PSO Parameters
k = 0.5;
kdamp = 0.75;

w=2;            % Inertia Weight 慣性權重
wdamp=0.90;     % Inertia Weight Damping Ratio

c1=2;         % Personal Learning Coefficient
c2=2;         % Global Learning Coefficient

maxI = 1000;
%% propose
mode = 1;
BestCost_arr_propose = zeros(MaxIt,1);
for i = 1:maxI
    disp(num2str(i));
    [BestSol,BestCost_propose] = pso_opt_fun(nVar,VarMin,VarMax,MaxIt,nPop,k,kdamp,w,wdamp,c1,c2,mode);
    BestCost_arr_propose = BestCost_arr_propose + BestCost_propose;
end
%% conv
mode =2;
BestCost_arr_conv = zeros(MaxIt,1);

for i = 1:maxI
    disp(num2str(i));
    [BestSol,BestCost_conv] = pso_opt_fun(nVar,VarMin,VarMax,MaxIt,nPop,k,kdamp,w,wdamp,c1,c2,mode);
    BestCost_arr_conv = BestCost_arr_conv + BestCost_conv;
end
%% Results

figure;
semilogx(BestCost_arr_conv/maxI,'LineWidth',1)
hold on;semilogx(BestCost_arr_propose/maxI,'LineWidth',1)
legend('Conv.','Propose')
xlabel('Iteration');
ylabel('Best Cost');
title('Conv. vs Propose w/ 1000 times simulation')
set(gca,'FontSize',15,'FontName','Times New Roman');




