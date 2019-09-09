function distanceToData = fmsFitToCurvePavlovianBlockingTaskPerCondition( conditions, vectParam )
%fmsFitToCurvePavlovianBlockingTastPerCondition
    
    %       vectParam contains:
        %       init = initial stimulus values (V_0)
        %       alpha = learning rate
        %       alpha2 = forgetting rate
        %       alpha3 = learning rate for no-reward
        %       alpha4 = learning rate stage 2 (different motivation)
        %       alpha5 = learning rate during muscimol injection
        %       motiv2 = boost in motivation (V bonus) at beginning of stage 2
        %       factorVtoMagEntries = this is multiplied to V to determine the number of magazine entries
    init = vectParam(1);
    alpha = vectParam(2);
    alpha2 = vectParam(3);
    alpha3 = alpha;
    alpha4 = alpha;
    alpha5 = alpha;
    motiv2 = vectParam(4);
    factor = vectParam(5);
    plotFigure = false;
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % launching the simulation of the Pavlovian Blocking task with the required parameters
    distanceToData = 0;
    if (conditions(1)) % saline
        distanceToData = distanceToData + launchSimRLonPavlovianBlockingTask(plotFigure, 1, init, alpha, alpha2, alpha3, alpha4, alpha5, motiv2, factor);
    end
    if (conditions(2)) % muscimol
        distanceToData = distanceToData + launchSimRLonPavlovianBlockingTask(plotFigure, 0, init, alpha, alpha2, alpha3, alpha4, alpha5, motiv2, factor);
    end
end

