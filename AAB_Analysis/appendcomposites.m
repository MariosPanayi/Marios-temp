function [data] = appendcomposites(data, composites, composites_comp)
% append composite body parts to end of data.
% e.g. adding a composite that si the average of 4 body parts (avg)
% 
% avg = 13;
% composites = [avg, head, midear, avg2pts];
% avg_comp = [nose tail rightear leftear];
% composites_comp = {avg_comp; head_comp; midear_comp; avg2pts_comp};

x = 1;
y = 2;
likelihood = 3;

temp_x = [];
temp_y = [];
temp_likelihood = [];
for i = 1: length(composites)
    columns = composites_comp{i};
    columns_x = ((columns-1)*3 + x +1);
    columns_y = ((columns-1)*3 + y +1);
    columns_likelihood = ((columns-1)*3 + likelihood +1);
    for j = 1:length(columns)
        temp_x(:,j) = data(:, columns_x(j));
        temp_y(:,j) = data(:, columns_y(j));
        temp_likelihood(:,j) = data(:, columns_likelihood(j));
    end
       
    composites_x = (((composites(i)-1)*3) + x +1);
    composites_y = (((composites(i)-1)*3) + y +1);   
    composites_likelihood = (((composites(i)-1)*3) + likelihood +1);   
    
    data(:,composites_x) = mean(temp_x,2);
    data(:,composites_y) = mean(temp_y,2);
    data(:,composites_likelihood) = mean(temp_likelihood,2);
    
temp_x = [];
temp_y = [];
temp_likelihood = [];
end

