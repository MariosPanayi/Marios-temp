function [Data] = interpolateLowConfidence2(data,criterion, filterparts)

x = ((filterparts-1)*3)+1+1;
y = ((filterparts-1)*3)+2+1;
confidence = ((filterparts-1)*3)+3+1;
for i = filterparts
    temp = data(:,confidence(i)) < criterion;
    data(temp,x(i)) = nan;
    data(temp,y(i)) = nan;
end
Data = fillmissing(data, 'linear', 1);