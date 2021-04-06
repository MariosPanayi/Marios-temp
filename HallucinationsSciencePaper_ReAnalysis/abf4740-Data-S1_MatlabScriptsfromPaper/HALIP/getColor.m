function colors=GetColor(strings)
% This function is a lookup table for the colors used in the figures of
% "Striatal Dopamine Mediates Hallucination-Like Perception in Mice" by
% Schmack et al. 2020. KS, April 2020, Cold Spring Harbor, schmack@cshl.edu
if ~iscell(strings)
    strings={strings};
end
for k=1:length(strings)
    switch strings{k}
        case {'calibration'}
            colors(k,:)=[.4 .4 .4];
            
        case 'hit'
            colors(k,:)=[61,221,195]./255;%#49EDC9
            
        case 'correctReject'
            colors(k,:)=[17,160,170]./255;
            
        case 'correct'
            colors(k,:)=[0.1529    0.7471    0.7157];
            
        case {'miss'}
            colors(k,:)=[76,62,118]./255;
            
        case {'ketamineHigh','falseAlarm'}
            colors(k,:)=[161,19,178]./255;
            
        case {'error'}
            colors(k,:)=[0.4647    0.1588    0.5804];
            
        case {'neutralBold'}
            colors(k,:)=[.4 .4 .4];
            
        case {'neutralLight'}
            colors(k,:)=[.7 .7 .7];

        case {'ketamineLow'}
            colors(k,:)=[235,112,213]./255;
            
        case {'noise'}
            c=cbrewer('div','PuOr',255,'pchip');
            c=copper(10);
            colors(k,:)=c(2,:);
            colors(k,:)=[51,25,0]./255;
            
        case {'signal'}
            c=cbrewer('div','PuOr',255,'pchip');
            colors(k,:)=c(1,:);
            colors(k,:)=[255,153,51]./255;
                        
        case {'biasHigh'}
            colors(k,:)=[14,31,124]./255;
            
        case {'biasMedium','conditionedHigh','signalChoice'}
            colors(k,:)=[51,78,220]./255;%#334EDC
            
        case 'optoOn'
            colors(k,:)=[101,122,238]./255;%#334EDC
            
        case 'optoOff'
            colors(k,:)=[.5 .5 .5];
            
        case {'biasLow','conditionedLow'} 
            colors(k,:)=[97,163,220]./255;%#61A3DC
            
        case {'neutralExtraBold'}
            colors(k,:)=[.2 .2 .2];
            
        case {'neutralExtraLight'}
            colors(k,:)=[.9 .9 .9];
            
        case 'singleLine'
            colors(k,:)=[.7 .7 .7];

            
        otherwise
            warning('%s not found.\n',strings{k})
            
            
    end
end