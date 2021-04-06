% This script \Cold Spring Harbor schmack@cshl.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - psychometric
% B-D calibration, vevaiometric, conditioned
% E - group data confidence
% F-H - correlations FA rate, FA confidence, high confidenc FA rate with CAPS
% I - correlations FA rate, FA confidence with psychopathology
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT NOTE: Some of the analyses include stochastic simulations (B-E, H).
% Therefore, results can vary slightly from run to run. For exactly
% repeatable results, set the random generator (e.g. 'randomSeedNumber=1')
% to a fixed value.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;
randomSeedNumber=now;

%% prelude
% initialize figure and caption
set(0,'defaultAxesFontName','Calibri','defaultTextFontName','Calibri','defaultFigureColor','w','DefaultLegendAutoUpdate','off','defaultAxesTickDir','out','defaultAxesTickDirMode', 'manual','defaultAxesTickLength',[.02 .02]);%set defaults
dataName='humanData.mat';
figureName='results\FigureHumanHallucinations.pdf';
captionName='results\StatsHumanHallucinations.txt';
if ~exist('results','dir')
    mkdir('results');
end
fid=fopen(captionName,'w');

% layout
rowFactor=4;
colFactor=5;
cols=4*colFactor;
rows=4*rowFactor;%columns and rows in figure
wid_cm=13;%figure width in cm; Science either 12cm or 5.5cm
px2cm = 1/96 * 2.54;
figHdl=figure('Name','Hallucinations & HALIPs','NumberTitle','off','Position',[0 100 ceil(wid_cm/px2cm) ceil(wid_cm/px2cm)/(cols/colFactor)*rows/rowFactor],'Visible','on');
t=tiledlayout(rows,cols,'TileSpacing','compact','Padding','compact');
k=0;

%plot options
markerSize=2;
minDataPoints=3;
binNumber=4;
binEdgesEvidence=[-5 0:2.5:10];

%% load and filter data
load(dataName,'subjectTab','sessionTab','trialTab');

%% place holder for task schematic
ph=nexttile([1*rowFactor,3*colFactor]);
ph.Visible='off';

%% A - psychometric
% prepare subplot
k=k+1;
h(k)=nexttile([rowFactor,colFactor]);

%prepare group data
binEdges=[-15,-1,0:2:10];
trialTab.bins=discretize(trialTab.evidence,binEdges);
subIdx=~isnan(trialTab.choice);
resTab=grpstats(trialTab(subIdx,:),{'bins','subjectId'},'nanmean','dataVars',{'evidence','choice'});
resTab.Properties.VariableNames=strrep(resTab.Properties.VariableNames,'nanmean_','mean_');
data=grpstats(resTab,{'bins'},{'mean','std'},'dataVars',{'mean_evidence','mean_choice'});
data.Properties.VariableNames=strrep(data.Properties.VariableNames,'mean_','');

%fit psychometric
[fitresult,gof] = psychometricModel(data,'genModel','guessGaussian');

%plot pychometric
psychometricPlot(fitresult,data,'axesHandle',h(k),...
    'xLabel','Signal-to-noise (dB)','yLabel','Signal choice (%)','legend','off','binEdges',...
    binEdges,'dataMarkerSize',20,'oddMarker','p','YPercent','on','dataMarkerSize',10,'fitBinEdges',[-20:.1:10]);
data.Properties.VariableNames=strrep(data.Properties.VariableNames,'sem_','std_');

%axes cosmetics
h(k).XTick=[-5,5,10];
h(k).YLim=[0 100];
h(k).XTickLabel={'No Signal','5','10'};
h(k).YTick=[0 50 100];
h(k).YLabel.Units='normalized';
h(k).Title.String='';
h(k).XLim(2)=11;

% denote false alarms (in response to Reviewer 01)
fac=getColor({'falseAlarm'});
xval=data.evidence(1);
yval=data.choice(1)*100+data.std_choice(1)*100;
yr=range(h(k).YLim);
xr=range(h(k).XLim);
x=[xval xval xval-.02*xr xval-.05*xr];
y=[yval+.08*yr yval+.18*yr yval+.24*yr yval+.34*yr];
plot(h(k),x(1:2),y(1:2),'Color',fac);
plot(h(k),x(1),y(1),'v','MarkerSize',5,'MarkerFaceColor',fac,'MarkerEdgeColor','none');
pan{k}(1)=text(h(k),x(3),y(3),sprintf('FA'),'Color',fac,'FontSize',8,'HorizontalAlignment','left');
pan{k}(2)=text(h(k),x(4),y(4),sprintf('%2.0f%s',data.choice(1)*100,'%'),'Color',fac,'FontSize',8,'HorizontalAlignment','left');

% add sample Size
hdl{k}=addSampleSize(trialTab,h(k),'southeast',4);
for ah=1:length(hdl)
    hdl{k}(ah).Position(1)=1;
end

%% add psychometric statistics to caption
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%\nRelation between Hallucinations and HALIPs\n%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%d humans, %d sessions, %d trials\n',...
    height(subjectTab),height(sessionTab),height(trialTab));
fprintf(fid,'\n1. Choice and Confidence Behavior\n\n');
fprintf(fid,'False Alarm Rate');
fprintf(fid,'%2.0f%s %s %2.2f%s',data.choice(1)*100,'%',char(177),data.std_choice(1)*100,'%');
fprintf(fid,'(mean %s std) \n',char(177));
fprintf(fid,'Variance explained by psychometric %2.2f%s\n\n',gof.rsquare*100,'%');

%% Figure 3C-F
% denote example worker to plot
exampleSubject='H1637';
subjects=unique(trialTab.subjectId);
allConfidenceGof=table;

% simulate or load
promptMessage = sprintf('Need statistical confidence simulation for each subject.\nLoad existing simulation or perform new simulation?\n(If existing simulation is not found, new simulation is performed in any case.)'); 
button = questdlg(promptMessage, 'Load', 'Load', 'Simulate', 'Load');
if ~exist('confidenceSimulation','dir')
    mkdir('confidenceSimulation');
end


for s=1:length(subjects)
    confidencePath=fullfile('confidenceSimulation',[sprintf(subjects{s}) '.mat']);
    if ~exist(confidencePath,'file')||strcmpi(button,'Simulate')
        %% print progress
        fprintf('Simulating statistical confidence %s (%d/%d)...',subjects{s},s,length(subjects));
        
        %% prepare data
        subIdx=strcmp(trialTab.subjectId,subjects{s})&~isnan(trialTab.choice);
        data=trialTab(subIdx,:);
        
        %% predict statistical confidence
        % infer "evidence level" for no signal trials
        data.oddtrial=data.evidence<0;
        [fitresult,~] = psychometricModel(data,'genModel','gaussian');
        oddEvidence=norminv(mean(data.choice(data.oddtrial==1)),fitresult.m,fitresult.sigma);%set non-fitted noise trials to predicted evidence
        
        % run model simulation
        randomseed=rng(randomSeedNumber);%for reproducibility, save seed random generator
        [simDataBoot,confidenceRawData] = confidenceModel(data,fitresult,'type','bootstrap',...
            'genModel','gaussian','plotMatch',false,'perceptBin','equalFill','nsim',1E3,'oddEvidence',oddEvidence);
        
        % organize data to be clear about evidence (realEvidence: assume -40dB
        % at no-signal trials, evidence: assume evidence estimated by
        % oddEvidence)
        data.confidenceRaw=confidenceRawData; % confidence in probability units
        data.realEvidence=data.evidence; %-5 noise evidence
        data.modelEvidence=data.evidence;% inferred noise evidence
        data.modelEvidence(data.oddtrial)=repmat(oddEvidence,sum(data.oddtrial),1);
        simDataBoot.modelEvidence=simDataBoot.evidence;% inferred noise evidence
        data.evidence=data.realEvidence;
        simDataBoot.evidence=simDataBoot.realEvidence;
        
        %% calculate fit between statistical confidence and time investments
        [confidenceGof]=confidenceGoodnessOfFit(data,simDataBoot,'nPermutations',1000,'binNumbers',[3],'sampleOption','up');
        
        %% save
        save(confidencePath,'data','simDataBoot','confidenceGof','randomseed');
        
        %% print progress
        fprintf('Done\n');
        
    elseif strcmpi(button,'Load')
        fprintf('Loading statistical confidence %s (%d/%d)...',subjects{s},s,length(subjects));
        load(confidencePath,'confidenceGof');% load goodness of fit
        fprintf('Done\n');
    end
    idx=strcmp(confidenceGof.signature,'vevaiometric')&strcmp(confidenceGof.confidenceName,'confidenceRaw');
    allConfidenceGof=[allConfidenceGof;confidenceGof(idx,:)];
    
    %% B-D plot calibration, vevaiometric and conditioned psychometric for example subject
    if ismember(subjects(s),exampleSubject)
        if strcmpi(button,'Load')
            % load full data and simData if not simulated
            load(confidencePath,'data','simDataBoot');
        end
        
        %% B - calibration
        % prepare panel
        k=k+1;
        h(k)=nexttile([rowFactor,colFactor]);
        
        % plot
        calibrationPlot(simDataBoot,'axesHandle',h(k),'binEdges',-1:.1:1,'minDataPoints',minDataPoints,'legend','off');
        calibrationPlot(data,'axesHandle',h(k),'binNumber',binNumber,'plotSize',false,'minDataPoints',minDataPoints,...
            'YPercent','on','xlabel','Confidence rating','ylabel','Accuracy (%)','legend','off','dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','errorSide','two','sampleOption','up');
        
        %axes stuff
        h(k).YLim=[26 100];
        h(k).XLim=[-.1 1];
        h(k).Title.String='';
        h(k).YTick=[50,75,100];
        h(k).YLabel.Units='normalized';
        
        % add sample size
        hdl{k}=addSampleSize( data(:,'subjectId'),h(k),'southeast',3);
        for ah=1:length(hdl{k})
            hdl{k}(ah).Position(1)=1;
        end
        
        %% C - vevaiometric
        % prepare panel & bins
        k=k+1;
        h(k)=nexttile([rowFactor,colFactor]);
        binEdgesEvidence=[-6 -4 0:2:10];
        
        %plot model
        vevaiometricPlot(simDataBoot,'axesHandle',h(k),'legend','off',...
            'binEdges',binEdgesEvidence,'fitLineStyle','-','minDataPoints',10,...
            'fitColors',getColor({'correct','error'}));
        %plot data in signal colors
        vevaiometricPlot(data,'axesHandle',h(k),'legend','off','binEdges',binEdgesEvidence,'plotSize',false,'minDataPoints',2,...
            'dataColors',getColor({'hit','miss'}),...
            'xLabel','Signal-to-noise (dB)','yLabel','Confidence rating','dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','errorSide','two');
        
        %plot data in no-signal colors
        vevaiometricPlot(data,'axesHandle',h(k),'legend','off','binEdges',binEdgesEvidence,'plotSize',false,'minDataPoints',2,...
            'dataColors',getColor({'correctReject','falseAlarm'}),...
            'xLabel','Signal-to-noise (dB)','yLabel','Confidence rating','dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','errorSide','two');
        
        
        %remove data points with 'wrong' colors (hits in no-signal colors
        %etc)
        a=findobj(h(k),'Color',getColor({'hit'}));
        a.YData(1)=nan;
        a=findobj(h(k),'Color',getColor({'correctReject'}));
        a.YData(2:end)=nan;
        a=findobj(h(k),'Color',getColor({'miss'}));
        a.YData(1)=nan;
        a=findobj(h(k),'Color',getColor({'falseAlarm'}));
        a.YData(2:end)=nan;
        
        %add legend (extra script, because complex)
        y=[.94,.8];
        x=[.03,.03];
        vevaiometricLegend(h(k),x,y);
        
        %axes cosmetics
        a=findobj(h(k),'Color',getColor({'falseAlarm'}));
        h(k).XTick=[a(1).XData(1),5,10];
        h(k).XTickLabel={'No Signal','5','10'};
        h(k).Title.String='';
        
        
        %% D - conditioned psychometric
        % prepare panel and bins
        k=k+1;
        h(k)=nexttile([rowFactor,colFactor]);
        binEdgesEvidence=[-5 0:2.5:10];
        
        %plot
        conditionedPlot(simDataBoot,'axesHandle',h(k),'legend','off','binEdges',-15:1:10,'fitLineStyle','-','prctileCut',50,'minDataPoints',minDataPoints);
        conditionedPlot(data,'axesHandle',h(k),'legend','off','binEdges',binEdgesEvidence,'minDataPoints',2,...
            'YPercent','on','xLabel','Signal-to-noise (dB)','yLabel','Signal choice','prctileCut',50,'dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','errorSide','two','sampleOption','up');
        
        %axes cosmetics
        a=get(h(k),'Children');
        h(k).XTick=[a(1).XData(1),5,10];
        h(k).XTickLabel={'No Signal','5','10'};
        h(k).Title.String='';
        h(k).YLabel.Units='normalized';
        
        % legend (manual, automatic does not work nicely)
        a=findobj(h(k),'Marker','.');
        [~,highIdx]=min([a(1).Color(1),a(2).Color(1)]);
        highProperties=a(highIdx);
        [~,lowIdx]=max([a(1).Color(1),a(2).Color(1)]);
        lowProperties=a(lowIdx);
        pan{k}(1)=text(h(k),1.22,.96-.74,'High conf.','FontSize',8,'Color',highProperties.Color,'Units','normalized','HorizontalAlignment','right');
        pan{k}(2)=text(h(k),1.22,.82-.74,'Low conf.','FontSize',8,'Color',lowProperties.Color,'Units','normalized','HorizontalAlignment','right');
    end
end

%% E - summary data
% prepare subplot
k=k+1;
h(k)=nexttile([rowFactor,colFactor]);

% prepare data
thisConfidenceGof=allConfidenceGof;
ev=(thisConfidenceGof.PearsonR).^2;
sev=nanmedian(thisConfidenceGof.PearsonR_Permutation.^2,2);
y=[ev;sev];
x=[zeros(size(ev));zeros(size(ev))+4];

% plot beeswarm
beeswarm(x,y,'Colormap',getColor({'neutralBold','neutralLight'}),'use_current_axes',0,'dot_size',markerSize/36,'MarkerFaceAlpha',1,'Marker','o','corral_style','none',...
    'sort_style','none');

% axis cosmetics
h(k).XLim=[-2 7];
h(k).XTick=[0 4];
h(k).YLabel.String={'Variance expl. (R^2)'};
h(k).YLabel.Interpreter='tex';
h(k).XTickLabel={'Predicted\newlineconfidence','Shuffle'};
h(k).XTickLabelRotation=0;

% add sample size
hdl{k}=addSampleSize(trialTab,h(k),'northeast',4);
for ah=1:length(hdl{k})
    hdl{k}(ah).Position(1)=1;
end
delete(hdl{k}(3:4));

%% add statistical confidence statistics to caption
fprintf(fid,'Statistical confidence and confidence ratings\n');
fprintf(fid,'Explained variance %2.0f%s, %2.0f - %2.0f [median, interquartile distance])\n',prctile(ev,[50])*100,'%',prctile(ev,[25,75])*100);
fprintf(fid,'p<0.05 in n=%d/%d (%2.0f%s)\n',sum(thisConfidenceGof.PearsonP_Permutation<0.05),height(thisConfidenceGof),sum(thisConfidenceGof.PearsonP_Permutation<0.05)./height(thisConfidenceGof)*100,'%');
fprintf(fid,'p<0.10 in n=%d/%d (%2.0f%s)\n',sum(thisConfidenceGof.PearsonP_Permutation<0.1),height(thisConfidenceGof),sum(thisConfidenceGof.PearsonP_Permutation<0.1)./height(thisConfidenceGof)*100,'%');
fprintf(fid,'p<0.20 in n=%d/%d (%2.0f%s)\n\n',sum(thisConfidenceGof.PearsonP_Permutation<0.2),height(thisConfidenceGof),sum(thisConfidenceGof.PearsonP_Permutation<0.2)./height(thisConfidenceGof)*100,'%');


%% F-H correlation with hallucinations
% calculate variables for correlations
% rescale confidence ratings to 0 and 1 for each participant (to account
% for individual differences in the use of the range of the confidence scale)
trialTab=grouptransform(trialTab,'subjectId','rescale','confidence');

% create variable for false alarm rate and false alarm confidence
signalIdx=trialTab.evidence>0;
nofaIdx=signalIdx|trialTab.choice==0;

% falseAlarmRate contains choices (0/1)*100 for no-signal trials and nans for signal
% trials. This is for calculating the false rate in %.
trialTab.falseAlarmRate=trialTab.choice.*100;
trialTab.falseAlarmRate(signalIdx)=nan;

% falseAlarmConfidence contains confidence ratings for no-signal trials and nans for signal
% trials. This is for calculating the false alarm confidence.
trialTab.falseAlarmConfidence=trialTab.confidence; % to account for individual differences in the usage of the scale
trialTab.falseAlarmConfidence(nofaIdx)=nan;

% split false alarms into high and low confidence false alarm based on
% median (as in conditioned psychometric)
medianConfidence=prctile(trialTab{:,'confidence'},50);
confHighIdx=trialTab.confidence>=medianConfidence;
confLowIdx=trialTab.confidence<medianConfidence;
trialTab.highFalseAlarmRate=nan(height(trialTab),1);
trialTab.lowFalseAlarmRate=nan(height(trialTab),1);
trialTab.highFalseAlarmRate(confHighIdx)=trialTab.falseAlarmRate(confHighIdx);
trialTab.lowFalseAlarmRate(confLowIdx)=trialTab.falseAlarmRate(confLowIdx);

% organize data
questVarNames={'capsScore'}';
questVarLabel={'Hallucinations','(CAPS score)'};
questVarLabelShort={'HALL'};

dataVarNames={'falseAlarmRate','falseAlarmConfidence','highFalseAlarmRate','lowFalseAlarmRate'};
dataVarLabel={'FA rate (%)','FA conf','High conf FA rate (%)','Low conf FA rate (%)'};
dataVarLabelShort={'FA rate','FA conf','HiConf FA rate','LoConf FA rate'};

sclVarNames=subjectTab.Properties.VariableNames(cellfun(@any,strfind(subjectTab.Properties.VariableNames,'scl')));
sclVarLabel=strrep(sclVarNames,'scl','');
sclVarLabelShort=sclVarLabel;

dataTab=grpstats(trialTab,'subjectId','nanmean','DataVars',dataVarNames);
dataTab.Properties.VariableNames=strrep(dataTab.Properties.VariableNames,'nanmean_','');
dataTab=join(dataTab,subjectTab);

% calculate correlations (Spearman)
d01=dataTab{:,questVarNames};
d02=dataTab{:,dataVarNames};
n=sum(~isnan(d01)&~isnan(d02));
[r,p]=corr(d01,d02,'rows','complete','Type','Spearman');

% prepare caption
fprintf(fid,'2. Correlations with hallucinations (CAPS)\n');
%% F-H: Hallucination proneness x False Alarm rate, False alarm confidence, High-confidence False alarm rate
for d=1:3
    
    % prepare panel
    k=k+1;
    h(k)=nexttile([1.5*rowFactor,colFactor]);
    hold(h(k),'on')
    
    % prepare data
    nanIdx=isnan(d01)|isnan(d02(:,d));
    x=d01(~nanIdx);
    y=d02(~nanIdx,d);
    
    % exclude outliers (only for plotting)
    if d==1||d==3
        upperBound=(prctile(d02(~nanIdx,d),.75)+5*(iqr(d02(~nanIdx,d))));
        lowerBound=(prctile(d02(~nanIdx,d),.25)-5*(iqr(d02(~nanIdx,d))));
        outlierIdx=d02(:,d)>upperBound|d02(:,d)<lowerBound;
        x=d01(~nanIdx&~outlierIdx);
        y=d02(~nanIdx&~outlierIdx,d);
    end
    
    % scatter raw data
    sortX = sort(x);%calculate rank for coloring
    [~, rnkX] = ismember(x,sortX);
    [~,sortIdx] = sort(x,'descend');
    scatter(x,y,markerSize*2,'Marker',' o','MarkerFaceColor',getColor({'neutralBold'}),'MarkerEdgeColor','none','MarkerFaceAlpha',.6);
    
    % plot non-parametric regression line
    l=plot(x(sortIdx(2:end-1)),smooth(x(sortIdx(2:end-1)),y(sortIdx(2:end-1)),range(x),'rlowess'));
    l.LineWidth=1;
    l.Color='k';%[0.2 0.2 0.2];
    
    % axis cosmetics
    h(k).YLabel.String=dataVarLabel{d};
    h(k).XLabel.String=questVarLabel;
    
    % significance text
    rhoString=getRhoString(r(d),p(d));
    rhoStringHdl{k}=text(h(k),0.55,1.05,rhoString,'FontSize',8,'Interpreter','latex','Units','normalized',...
        'HorizontalAlignment','center','Color','k');
    
    % manual limit adjustments
    if d==1
        h(k).YLim=[-1 22];
        h(k).XLim=[-15 275];
    elseif d==2
        h(k).Clipping='on';
        h(k).YLim=[0 1];
        h(k).XLim=[-15 275];
    elseif d==3
        h(k).XLim=[-15 275];
    end
    
    % add caption (also make sure that exclusion outliers would not affect
    % statistics)
    fprintf(fid,'\n%s x %s\nr=%2.2f p=%2.5f (n=%d, Spearman)\n',questVarNames{1},dataVarNames{d},r(d),p(d),n(d));
    fprintf(fid,'not plotted %d extreme outliers with values >%2.2f (75th percentile + 5 IQR)\n',sum(dataTab{:,dataVarNames{d}}>upperBound),upperBound);
    [rout,pout]=corr(x,y,'Type','Spearman');
    fprintf(fid,'correlation without outliers r=%2.2f p=%2.5f (n=%d)\n',rout,pout,length(x));
    fprintf(fid,'%d zero subjects\n',sum(y==0));
end

%% J other psychopathological domains x HALIP
% prepare data
allVarNames=[questVarNames,sclVarNames,dataVarNames([1:2])];
allVarLabels=[questVarLabelShort,sclVarLabelShort,dataVarLabelShort([1:2])];

corrMatrix=dataTab{:,allVarNames};
rankTable=varfun(@tiedrank,dataTab(:,allVarNames));
rankTable.Properties.VariableNames=strrep(rankTable.Properties.VariableNames,'tiedrank_','');
rankMatrix=rankTable{:,:};

% calculate correlation
[rho,p]=corr(corrMatrix,'Type','Spearman','rows','complete');

% organize data for plot
rowIdx=length(questVarNames)+length(sclVarNames)+1:size(rho,1);
colIdx=1:(length(questVarNames)+length(sclVarNames));
R_PxT=rho(rowIdx,colIdx);
P_PxT=p(rowIdx,colIdx);
row_PxT=allVarLabels(rowIdx);
col_PxT=allVarLabels(colIdx);
rowVar_PxT=allVarNames(rowIdx);
colVar_PxT=allVarNames(colIdx);

% placeholder
ph=nexttile([1 2*(colFactor-1)]);
ph.Visible='off';

% prepare panel
k=k+1;
h(k)=nexttile([1.5*rowFactor,colFactor-1]);
hold(h(k),'on')

% plot data
imagesc(R_PxT,[min(min(R_PxT(1:2,:))),max(max(R_PxT(1:2,:)))]);
colormap('cividis')

% asess significance in step-wise correlation and add stars to plot
mdl=stepwiselm(rankTable(:,[questVarNames,sclVarNames,{'falseAlarmRate'}]));
antab1=mdl.anova;
sigVars=antab1.Properties.RowNames(~strcmp(antab1.Properties.RowNames,'Error'));
x=find(ismember(colVar_PxT,sigVars));
y=ones(size(x));
plot(x,y,'k*','MarkerSize',5);

mdl=stepwiselm(rankTable(:,[questVarNames,sclVarNames,{'falseAlarmConfidence'}]));
antab2=mdl.anova;
sigVars=antab2.Properties.RowNames(~strcmp(antab2.Properties.RowNames,'Error'));
x=find(ismember(colVar_PxT,sigVars));
y=ones(size(x))+1;
plot(x,y,'k*','MarkerSize',5);

% axes cosmetics
h(k).YTick=1:length(row_PxT);
h(k).YTickLabel=row_PxT;
h(k).TickLabelInterpreter='none';
h(k).YTickLabelRotation=0;

h(k).XTick=1:length(col_PxT);
h(k).XTickLabel=col_PxT;
h(k).TickLabelInterpreter='none';
h(k).XTickLabelRotation=90;
h(k).YLabel.String='';

%swap x and y axis (for layout reasons)
view(90,90);
h(k).XTickLabelRotation=0;
h(k).YTickLabelRotation=45;
h(k).YLim=[.5 3];
h(k).XLim=[0.5 12.5];
% BUG: for some reason the X label does not work properly
% WORKAROUND: manual text
% h(k).XLabel.String={'Psychopathology (SCL90R score)'};
% h(k).XLabel.Units='normalized';
% h(k).XLabel.Position(2)=.4;
% h(k).XLabel.HorizontalAlignment='center';
pan{k}=text(h(k),-.8,1,'Psychopathology (SCL90R score)',...
    'FontSize',h(k).XLabel.FontSize,...
    'Color',h(k).XAxis.Color,...
    'HorizontalAlignment','right',...
    'Rotation',90,...
    'Units','normalized');

% make box around axes (manually)
r(1)=rectangle(h(k),'Position',[12,2.5,1,1],'EdgeColor','none','FaceColor','w');
r(2)=rectangle(h(k),'Position',[0,0,length(col_PxT)-.5,2-.5]+.5,'EdgeColor','k','FaceColor','none');

% add colorbar (manually, the MATLAB routine does not work nicely with
% tiledlayout)
% prepare space
k=k+1;
h(k)=nexttile([1.5*rowFactor 1]);

% prepare data
ymin=min(R_PxT(:));
ymax=max(R_PxT(:));
values=linspace(ymin,ymax,100);
yticks=(([0:0.05:1]-ymin)./(ymax-ymin))*99+1;
yticks=yticks(yticks<100&yticks>0);
xtl=(yticks-1)./99*(ymax-ymin)+ymin;

% plot
imagesc(repmat(values,100,1)',[min(values),max(values)]);
colormap('cividis');

%axis stuff
h(k).YDir='normal';
h(k).Clipping='off';
shift=200;%change this to move everything left or right
h(k).XLim=[0 10]+shift;
h(k).YLabel.String='';
h(k).YAxis.Color='none';
h(k).XAxis.Color='none';
h(k).YTick=yticks;
h(k).YTickLabel='';%cellfun(@num2str,num2cell(yticklabels),'UniformOutput',false);
for gg=1:length(h(k).YTick)
    t(gg)=text(-80+shift,h(k).YTick(gg),num2str(xtl(gg)),'HorizontalAlignment','left','FontSize',8);
end
t(end+1)=text(120+shift,50,'Spearman''s \rho','HorizontalAlignment','center','FontSize',8,'Rotation',90);

%% caption summary correlations
rho(abs(rho-1)<eps)=nan;

fprintf(fid,'\n3. Correlations with other psychopathology domains (SCL-90R)\n\n');
%psychopathology
fprintf(fid,'hallucinations x psychopathology \nmin rho %2.2f max rho %2.2f\n\n',...
    max(max(rho(colIdx(1),colIdx(2:end)))),min(min(rho(colIdx(1),colIdx(2:end)))));

fprintf(fid,'psychopathology intercorrelations\nmin rho %2.2f max rho %2.2f\n\n',...
    max(max(rho(colIdx(2:end),colIdx(2:end)))),min(min(rho(colIdx(2:end),colIdx(2:end)))));

fprintf(fid,'other psychopathology with false alarm rate and confidence\nmin rho %2.2f max rho %2.2f\n\n',...
    max(max(rho(rowIdx,colIdx(2:end)))),min(min(rho(rowIdx,colIdx(2:end)))));

%caption content stepwise
fprintf(fid,'stepwise regression false alarm rate (add p<0.05, removal p>0.1):\n');
for m=1:2:height(antab1)
    fprintf(fid,'%s: F(%d,%d)=%2.2f p=%2.5f\n\n',antab1.Properties.RowNames{m},antab1.DF(m),antab1.DF(m+1),antab1.F(m),antab1.pValue(m));
end

fprintf(fid,'stepwise regression false alarm confidence (add p<0.05, removal p>0.1):\n');
for m=1:2:height(antab2)
    fprintf(fid,'%s: F(%d,%d)=%2.2f p=%2.5f\n\n',antab1.Properties.RowNames{m},antab2.DF(m),antab2.DF(m+1),antab2.F(m),antab2.pValue(m));
end

%% figure finishing
%put letters
toAnnotate=[1:10];
for a=1:length(toAnnotate)
    k=toAnnotate(a);
    an(a)=annotation('textbox','String',char('A'+a-1));
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    an(a).HorizontalAlignment='center';
    an(a).VerticalAlignment='middle';
    an(a).EdgeColor='none';
    an(a).Margin=0;
    an(a).FontWeight='bold';
    an(a).FontSize=10;
end

% re-adjust position letters
x=[0.02 .25 .49 .73];
y=[.98 .73 .46];
for a=1:10
    k=toAnnotate(a);
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    [~,xIdx]=min(abs(x-an(a).Position(1)));
    [~,yIdx]=min(abs(y-an(a).Position(2)));
    an(a).Position=[x(xIdx) y(yIdx) 0 0];
end
an(a).Position=[x(end) y(end) 0 0];

% text realignments
k=['A']-64;
for n=1:2
    hdl{k}(n).Position(1)=0.04;
    hdl{k}(n).Position(2)=1.14-n*0.14;
    hdl{k}(n).HorizontalAlignment='Left';
end
for n=3:4
    hdl{k}(n).Position(1)=1.05;
end
pan{k}(1).Position(2)=48;
pan{k}(2).Position(2)=36;

k=['C']-64;
h(k).XLim=[ -6   11];

k=['D']-64;
h(k).XLim=[ -6   11];
h(k).YLim=[0 100];
for n=1:2
    pan{k}(n).Position(1)=1.2;
end

k='E'-64;
h(k).XTickLabel{1}='';
h(k).XTickLabel{2}='';
t(1)=text(h(k),-.8,-.14,'Predicted','Units','Data','HorizontalAlignment','Center','FontSize',h(k).XAxis.FontSize,'Color',h(k).XAxis.Color);
t(2)=text(h(k),-.8,-.29,'Confidence','Units','Data','HorizontalAlignment','Center','FontSize',h(k).XAxis.FontSize,'Color',h(k).XAxis.Color);
t(3)=text(h(k),4.8,-.14,'Shuffle','Units','Data','HorizontalAlignment','Center','FontSize',h(k).XAxis.FontSize,'Color',h(k).XAxis.Color);

k='F'-64;
h(k).YLabel.String='FA rate (%)';
rhoStringHdl{k}.Position=[.7 .9];

k='G'-64;
h(k).YLabel.String='FA confidence rating (%)';
rhoStringHdl{k}.Position=[.7 .9];

k='H'-64;
h(k).YLabel.String='High confidence FA rate (%)';
rhoStringHdl{k}.Position=[.7 .9];

%% save figure and caption
% BUG: png export does not work properly! two options:
% 1. export svg and convert to png in inkscape:
% saveas(figHdl,strrep(figureName,'png','svg'));
% 2. export as pdf (the only figure format that turned out to work)
set(figHdl,'Visible','on');
exportgraphics(figHdl,figureName,'Resolution',900);
fprintf('Saved plots to %s.\n',figureName)
close(figHdl);

fprintf('Saved statistics to %s.\n',captionName)
fclose(fid);


function vevaiometricLegend(h,x,y)
if nargin<3
    y=[.22,.08];
    if nargin<2
        x=[.02,.02];
    end
end
word='xxCorrect';
string='';
colors=[getColor('correctReject');getColor('hit');repmat(getColor('correct'),length(word)-2,1)];

for c=1:size(colors,1)
    string=[string,sprintf('%scolor[rgb]{%2.4f,%2.4f,%2.4f}%s','\',colors(c,:),word(c))];
end
string=strrep(string,'x','\bullet');
text(h,x(1),y(1),string,'FontSize',8,'Interpreter','tex','Units','normalized','HorizontalAlignment','left');

word='xxError';
string='';
colors=[getColor('falseAlarm');getColor('miss');repmat(getColor('error'),length(word)-2,1)];

for c=1:size(colors,1)
    string=[string,sprintf('%scolor[rgb]{%2.4f,%2.4f,%2.4f}%s','\',colors(c,:),word(c))];
end
string=strrep(string,'x','\bullet');

text(h,x(2),y(2),string,'FontSize',8,'Interpreter','tex','Units','normalized','HorizontalAlignment','left');
end

function rhoString=getRhoString(r,p)

if p<0.001
    pstring='***';
elseif p<0.01&&p>0.001
    pstring='**';
elseif p<0.05&&p>0.01
    pstring='*';
else
    pstring='ns';
end
rhoString=sprintf('$\\rho$=%2.2f%s',r,pstring);
end
