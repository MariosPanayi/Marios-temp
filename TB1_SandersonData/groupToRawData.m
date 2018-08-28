function [datastack] = groupToRawData(rawData, groupData, groupIndex, index)

for i = 1:size(rawData, 1)
datastack(i) = rawData(i) - groupData(groupindex == index(i))
end

end