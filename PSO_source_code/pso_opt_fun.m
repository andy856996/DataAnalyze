
function [BestSol,BestCost] = pso_opt_fun(nVar,VarMin,VarMax,MaxIt,nPop,k,kdamp,w,wdamp,c1,c2,mode)
%% Problem Definition
%CostFunction = @(x, y) 20 + x.^2 + y.^2 - 10*(cos(2*pi*x) + cos(2*pi*y));
%CostFunction=@(x) Sphere(x);        % Cost Function
%nVar=1;            % Number of Decision Variables
VarSize=[2 nVar];   % Size of Decision Variables Matrix
%VarMin=-70;         % Lower Bound of Variables
%VarMax= 130;         % Upper Bound of Variables
%% PSO Parameters
% MaxIt=120;      % Maximum Number of Iterations
% nPop=20;        % Population Size (Swarm Size)
% % PSO Parameters
% k = 2;
% kdamp = 0.75;
% w=2;            % Inertia Weight 慣性權重
% wdamp=0.75;     % Inertia Weight Damping Ratio
% c1=2;         % Personal Learning Coefficient
% c2=2.0;         % Global Learning Coefficient
% mode = 2;
%% Initialization
empty_particle.Position=[];
empty_particle.Cost=[];
empty_particle.Velocity=[];
empty_particle.Best.Position=[];
empty_particle.Best.Cost=[];
particle=repmat(empty_particle,nPop,1);
GlobalBest.Cost=inf;
for i=1:nPop
    % Initialize Position
    particle(i).Position=unifrnd(VarMin,VarMax,VarSize);
    % Initialize Velocity
    particle(i).Velocity=zeros(VarSize);
    % Evaluation
    particle(i).Cost=CostFunction(particle(i).Position);
    % Update Personal Best
    particle(i).Best.Position=particle(i).Position;
    particle(i).Best.Cost=particle(i).Cost;
    % Update Global Best
    if particle(i).Best.Cost<GlobalBest.Cost
        GlobalBest=particle(i).Best;
    end
end
BestCost=zeros(MaxIt,1);
%% PSO Main Loop
for it=1:MaxIt
    for i=1:nPop
        %% Update Velocity
%         particle(i).Velocity = w*particle(i).Velocity ...
%             +c1*rand(VarSize).*(particle(i).Best.Position-particle(i).Position) ...
%             +c2*rand(VarSize).*(GlobalBest.Position-particle(i).Position);
        if mode == 1
            a = 0;b = 1.5;
            w1 = w*(a + (b-a).*rand());
            one =  w1*particle(i).Velocity;
            two = c1*rand().*(particle(i).Best.Position-particle(i).Position) ;
            three = c2*rand().*(GlobalBest.Position-particle(i).Position);
            %four = k*(one + two + three);
            %four = k*(VarMax - VarMin);
            %particle(i).Velocity = one + two + three + four;
            particle(i).Velocity = one + two + three;
        else
            one =  w*particle(i).Velocity;
            two = c1*rand().*(particle(i).Best.Position-particle(i).Position) ;
            three = c2*rand().*(GlobalBest.Position-particle(i).Position);
            particle(i).Velocity = one + two + three;
        end
        %% Apply Velocity Limits
        % particle(i).Velocity = max(particle(i).Velocity,VelMin);
        % particle(i).Velocity = min(particle(i).Velocity,VelMax);
        %% Update Position
        particle(i).Position = particle(i).Position + particle(i).Velocity;
        %% Velocity Mirror Effect
        %IsOutside=(particle(i).Position<VarMin | particle(i).Position>VarMax);
        %particle(i).Velocity(IsOutside)=-particle(i).Velocity(IsOutside);
        %% Apply Position Limits
        particle(i).Position = max(particle(i).Position,VarMin);
        particle(i).Position = min(particle(i).Position,VarMax);
        %% Evaluation
        particle(i).Cost = CostFunction(particle(i).Position);
        %% Update Personal Best
        if particle(i).Cost<particle(i).Best.Cost
            particle(i).Best.Position=particle(i).Position;
            particle(i).Best.Cost=particle(i).Cost;
            % Update Global Best
            if particle(i).Best.Cost<GlobalBest.Cost
                GlobalBest=particle(i).Best;
            end
        end
    end
    BestCost(it)=GlobalBest.Cost;
    %disp(['Iteration ' num2str(it) ': Best Cost = ' num2str(BestCost(it))]);
    w=w*wdamp;
    if mode ==1
        k=k*kdamp;
    end
end
BestSol = GlobalBest;
end
function E =  CostFunction_(x)
E = 20 +x(1).^2 + x(2).^2 - 10*(cos(2*pi*x(1)) + cos(2*pi*x(2)));
end

function z=Sphere(x)

    z=sum(x.^2);

end

function [y] = CostFunction(xx)

x1 = xx(1);
x2 = xx(2);

fact1 = sin(x1)*cos(x2);
fact2 = exp(abs(1 - sqrt(x1^2+x2^2)/pi));

y = -abs(fact1*fact2);

end