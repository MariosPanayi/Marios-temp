function maze_poly = create_maze_poly(pos_data)

%take in a matrix, take first 3 columns as data
x = pos_data(:,2);
y = pos_data(:,3);

%plot data
figure 
hold on
plot(x, y, 'color', [.7 .7 .7],'LineWidth',1)
plot(x, y, 'bo')

%zoom out a bit
xmin = min(pos_data(:,2));
xmax =  max(pos_data(:,2));
ymin = min(pos_data(:,3));
ymax =  max(pos_data(:,3));
xdiff = xmax-xmin;
ydiff = ymax-ymin;
xlim([xmin-(xdiff*0.1) xmax+(xdiff*0.1)])
ylim([ymin-(ydiff*0.1) ymax+(ydiff*0.1)])


%click corners of maze (use method from mql)
h = impoly;
maze_poly = getPosition(h);

maze_poly(size(maze_poly,1)+1,:) = maze_poly(1,:); 