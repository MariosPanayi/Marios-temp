function textHdl=addSampleSize(allData,h,position,lines)
% this helper function adds a string describing the sample size in allDatt 
% to a subplot 
% KS April 2020, Cold Spring Harbor
% schmack@cshl.edu
%
% inputs (mandatory)
% allData    -  a table with variable 'subjectId' or 'worker_index' to denote
%            different mice (subjectId) or humans (workerId), or
%            'modelEvidence' to denote that this is data simulated by a
%            model
% h          - handle to the axis where the string should be placed
% position   - position of string: 'southeast'(default),'southwest',
%           'northeast','northwest' or 'north')
% lines     - how much information (subjects/trials) will be plotted over how many lines
%           (2-4)
if nargin<4
    lines=2;
end
if nargin<3
    position='southeast';
end
if any(~cellfun(@isempty,strfind(allData.subjectId,'K')))
    N=length(unique(allData.subjectId));
    species={'Mice','Mouse'};
elseif any(~cellfun(@isempty,strfind(allData.subjectId,'H')))
    N=length(unique(allData.subjectId));
    species={'Humans','Human'};
end

if N>1
    if lines==2
        numberStrings{1}=sprintf('N=%d %s',N,species{1});
        numberStrings{2}=[addComma(height(allData)) ' trials'];
    elseif lines==3
        numberStrings{1}=sprintf('N=%d %s',N,species{1});
        numberStrings{2}=[addComma(height(allData))];
        numberStrings{3}='trials';
    elseif lines==4
        numberStrings{1}=sprintf('N=%d',N);
        numberStrings{2}=sprintf('%s',species{1});        
        numberStrings{3}=[addComma(height(allData))];
        numberStrings{4}='trials';
    end
        
elseif N==1
    if any(ismember(allData.Properties.VariableNames,'modelEvidence'))
        numberStrings{1}=sprintf('Model');
    else
        if lines==2
            subject=unique(allData.subjectId);
            numberStrings{1}=sprintf('%s %s',species{2},subject{1});
            numberStrings{2}=[addComma(height(allData)) ' trials'];
            
        elseif lines==3
            try
            subject=unique(allData.subjectId);
            catch
                subject={sprintf('H%d',unique(allData.worker_index))};
            end
            numberStrings{1}=sprintf('%s',species{2});
            numberStrings{2}=sprintf('%s',subject{1});
            numberStrings{3}=[addComma(height(allData)) ' trials'];
        end
    end
end
switch position
    case 'southeast'
        x=.95;y=.08+(length(numberStrings)-1)*.14;
        align='right';     
            case 'southwest'
        x=.05;y=.08+(length(numberStrings)-1)*.14;
        align='left';

    case 'east'
        x=.95;
        y=.36+(length(numberStrings)-1)*.14;
        align='right';
    case 'northwest'
        x=.05;y=.92;
        align='left';
    case 'northeast'
        x=.95;y=.92;
        align='right';
        
end
d=-.14;
for ns=1:length(numberStrings)
    if ns==length(numberStrings)
        fs=8;
    else fs=8;
    end
    textHdl(ns)=text(h,x,y,numberStrings{ns},'FontSize',fs,'Color',h.XAxis.Color,'Units','normalized','HorizontalAlignment',align);
    y=y+d;
end
end