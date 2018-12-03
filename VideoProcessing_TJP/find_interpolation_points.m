function valid_pos_index = find_interpolation_points(posdata, maze_poly)
% function find_interpolation_points(posdata, maze_poly)
% Interpolates position data. Data is interpolated if it is outside of a
% user defined polygon

%find points outside of maze, using maze poly
xv = maze_poly(:,1);
yv = maze_poly(:,2);

rng default
x = posdata(:,2);
y = posdata(:,3);

in = inpolygon(x,y,xv,yv);

figure

plot(xv,yv,'LineWidth',2) % polygon
axis equal

hold on
plot(x(in),y(in),'r+') % points inside
plot(x(~in),y(~in),'bo') % points outside
hold off

%return index of valid data points
valid_pos_index = in;