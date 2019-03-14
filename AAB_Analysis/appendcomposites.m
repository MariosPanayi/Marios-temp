function [data] = appendcomposites(data, composites, composites_comp)
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
       
    composites_x = ((composites(i)*3) + x +1);
    composites_y = ((composites(i)*3) + y +1);   
    composites_likelihood = ((composites(i)*3) + likelihood +1);   
    
    data(:,composites_x) = mean(temp_x,2);
    data(:,composites_y) = mean(temp_y,2);
    data(:,composites_likelihood) = mean(temp_likelihood,2);
    
temp_x = [];
temp_y = [];
temp_likelihood = [];
end

