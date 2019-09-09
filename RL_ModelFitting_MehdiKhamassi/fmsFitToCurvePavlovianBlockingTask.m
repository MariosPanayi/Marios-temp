function results = fmsFitToCurvePavlovianBlockingTask( methode, conditions )
%fmsFitToCurvePavlovianBlockingTast performs an optimization of parameters minimizing the
%distance between the model's simulation curves and the curve of the
%experimental data in the Pavlovian Blocking Task of Marios Panayi and
%Simon Kilcross.
    
    tic
    
    tStart = tic;

    %       vectParam contains:
    %       init = initial stimulus values (V_0)
    %       alpha = learning rate
    %       alpha2 = forgetting rate
    %       alpha3 = learning rate for no-reward
    %       alpha4 = learning rate stage 2 (different motivation)
    %       alpha5 = learning rate during muscimol injection
    %       motiv2 = boost in motivation (V bonus) at beginning of stage 2
    nbFreeParam = 5;
    nbSample = 100;
    % initializing model fitting
    mygrid = [rand(1,nbSample); rand(1,nbSample); rand(1,nbSample); rand(1,nbSample); rand(1,nbSample)];  % init, alpha, alpha2, motiv2, factor
    minParam = [0 0 0 0 0];  % init, alpha, alpha2, motiv2, factor
    maxParam = [1 1 1 1 Inf];  % init, alpha, alpha2, motiv2, factor
    if (methode == 'fms')
        fmsResults = zeros(4+nbSample,nbFreeParam+1);
        % first testing extreme or prior parameter values
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.015, 0.08, 0, 0.015, 0.015, 0.1, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.015, 0.08, 0, 0.015, 0.015, 0.1, 5);
        fmsResults(nbSample+1,:) = [0 0.015 0.08 0.1 5 (distance+distance2)];
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.015, 0.1, 0, 0.015, 0.015, 0.2, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.015, 0.1, 0, 0.015, 0.015, 0.2, 5);
        fmsResults(nbSample+2,:) = [0 0.015 0.1 0.2 5 (distance+distance2)];
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.015, 0.15, 0, 0.015, 0.015, 0.15, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.015, 0.15, 0, 0.015, 0.015, 0.15, 5);
        fmsResults(nbSample+3,:) = [0 0.015 0.15 0.15 5 (distance+distance2)];
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.1, 0.05, 0, 0.1, 0.1, 0.5, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.1, 0.05, 0, 0.1, 0.1, 0.5, 5);
        fmsResults(nbSample+4,:) = [0 0.1 0.05 0.5 5 (distance+distance2)];
    end
    if (methode == 'fmc')
        options = optimset('Algorithm','interior-point');
        fmcResults = zeros(nbSample,nbFreeParam+1);
        gradient = zeros(nbSample,nbFreeParam);
        hessian = zeros(nbSample,nbFreeParam,nbFreeParam);
        % first testing extreme or prior parameter values
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.015, 0.08, 0, 0.015, 0.015, 0.1, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.015, 0.08, 0, 0.015, 0.015, 0.1, 5);
        fmcResults(nbSample+1,:) = [0 0.015 0.08 0.1 5 (distance+distance2)];
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.015, 0.1, 0, 0.015, 0.015, 0.2, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.015, 0.1, 0, 0.015, 0.015, 0.2, 5);
        fmcResults(nbSample+2,:) = [0 0.015 0.1 0.2 5 (distance+distance2)];
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.015, 0.15, 0, 0.015, 0.015, 0.15, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.015, 0.15, 0, 0.015, 0.015, 0.15, 5);
        fmcResults(nbSample+3,:) = [0 0.015 0.15 0.15 5 (distance+distance2)];
        distance = launchSimRLonPavlovianBlockingTask(false, 1, 0, 0.1, 0.05, 0, 0.1, 0.1, 0.5, 5);
        distance2 = launchSimRLonPavlovianBlockingTask(false, 0, 0, 0.1, 0.05, 0, 0.1, 0.1, 0.5, 5);
        fmcResults(nbSample+4,:) = [0 0.1 0.05 0.5 5 (distance+distance2)];
    end

    % launching model fitting
    niter = 1;
    for spl = 1:nbSample
        vectFreeParam = [mygrid(1,spl) mygrid(2,spl) mygrid(3,spl) mygrid(4,spl) mygrid(5,spl)]; % init, alpha, alpha2, motiv2, factor
        if (methode == 'fms')
            [x, fval] = fminsearch(@(x) fmsFitToCurvePavlovianBlockingTaskPerCondition(conditions, x), vectFreeParam(1:nbFreeParam));
            fmsResults(niter,:) = [x fval];
        end
        if (methode == 'fmc')
            [x, fval, ~, ~, ~, grad, hess] = fmincon(@(x) fmsFitToCurvePavlovianBlockingTaskPerCondition(conditions, x), vectFreeParam(1:nbFreeParam), [], [], [], [], minParam(1:nbFreeParam), maxParam(1:nbFreeParam), [], options);
            fmcResults(niter,:) = [x fval];
            gradient(niter,:) = grad';
            hessian(niter,:,:) = hess;
        end
        progress = [num2str(100 * niter / (nbSample)) '%']
        niter = niter + 1;
    end

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % save data
    if (methode == 'fms')
        %save(['PavlovBlockFitToCurve_Cond' num2str(nc-1) '_fmsResults'], 'sDATA', 'fmsResults');
        results = fmsResults;
    end
    if (methode == 'fmc')
        %save(['PavlovBlockFitToCurve_Cond' num2str(nc-1) '_fmcResults'], 'sDeATA', 'fmcResults','gradient','hessian');
        results = fmcResults;
    end

    % measuring duration
    tElapsed = toc(tStart);
    elapsedTimeForSubjectNumber = [num2str(tElapsed) ' seconds']
    
    toc
end

