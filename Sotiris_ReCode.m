CoachList = {'36521817'
'Kayleigh'
'Emily'
'Steven'
'Bex'
'Laure'
'Andy'
'Paulina'
'Tor'};

for i = 1:size(CoachList(1))
    for j = 1:size(data,1)
    coach{i,j} = contains(data{j}, CoachList{i},'IgnoreCase',true);
    end
end

coach  = coach';