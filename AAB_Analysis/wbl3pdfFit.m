function [phat] = wbl3pdfFit(data) 

custpdf = @(x,a,b,c) (x>c).*(b/a).*(((x-c)/a).^(b-1)).*exp(-((x-c)/a).^b);

datahist = [];
for i = 1:size(data,1)
  datahist = [datahist; ones(data(i)*1000,1)*i];    
end

opt = statset('MaxIter',1e5,'MaxFunEvals',1e5,'FunValCheck','off');

phat = mle(datahist,'pdf',custpdf,'start',[0 0 0],'Options',opt,...
    'LowerBound',[0 0 -Inf],'UpperBound',[Inf Inf min(data)]);
end

% Function explained in : https://mathworks.com/examples/statistics/mw/stats-ex78291222-estimate-parameters-of-a-three-parameter-weibull-distribution
% 3 parameter weibull
% data = vector of data to fit [rows or cols works]
% phat = [a b c] estimates of 3 parameter weibull [defined in custpdf]
%       a =  scale parameter i.e. height/width of curve [small values are tall and thin, large values are short and wide]
%       b = shape parameter i.e. shape (also known as the slopoe paramter)
%       when 0 < a < 1 shape is convex; 1 > a > 2.6 right skew; 2.6 < a 3.7 approximates normal curve, a > 3.7 left skew 
%       c =  location parameter i.e. delay to start 
