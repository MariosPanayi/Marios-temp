function formatSigHdl(sigHdl,h,offset)
% this helper function pretties up the significance star that is produced
% by sig star
% KS, January 2021, Cold Spring Harbor Laboratory, schmack@cshl.edu
if nargin<3
    offset=0;
end
yl=range(h.YLim);
for ss=1:size(sigHdl,1)
    sigHdl(ss,1).LineWidth=1;
    xl=sigHdl(ss,1).XData(end)-sigHdl(ss,1).XData(1);
    sigHdl(ss,1).YData=sigHdl(ss,1).YData+offset*range(h.YLim);
        sigHdl(ss,2).Position(2)=sigHdl(ss,2).Position(2)+offset*range(h.YLim);
    if strcmp(sigHdl(ss,2).String,'ns')||strcmp(sigHdl(ss,2).String,'n.s.')
        r=rectangle('Position',[sigHdl(ss,2).Position(1:2)-[0.3*xl,0.025*yl] 0.6*xl,0.05*yl],'EdgeColor','none','FaceColor','w');
        newHdl=text(sigHdl(ss,2).Position(1),sigHdl(ss,2).Position(2),sigHdl(ss,2).String,'FontSize',8,'HorizontalAlignment','center');
    else
    r=rectangle('Position',[sigHdl(ss,2).Position(1:2)-[0.25*xl,0.025*yl] 0.5*xl,0.05*yl],'EdgeColor','none','FaceColor','w');
        newHdl=text(sigHdl(ss,2).Position(1),sigHdl(ss,2).Position(2)-0.04*yl,sigHdl(ss,2).String,'FontSize',10,'HorizontalAlignment','center');
    end
    delete(sigHdl(ss,2));
    sigHdl(ss,2)=newHdl;
end
