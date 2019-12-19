function [bottomleft, bottomright, topleft, topright] = triangletorectangle(bottom1, bottom2, top)
%% Find points for the corners of a rectangle to detect activity within
% Analysis function for DeepLabCut tracking from MK001/MK002 Operant Box FCV in mice
% here I have 3 points defining the magazine, and need to convert this
% triangle into an appropriate rectangle
% a nd b are the base of the triangle
% Data format: each input and output should be [x,y] 
a = bottom1;
b = bottom2;
c = top;
x = 1;
y = 2;
% plot([a(x), b(x), c(x)], [a(y), b(y), c(y)])
  
ab = polyfit([a(x), b(x)]', [a(y), b(y)]',1);
ab_0 = ab(2);
ab_1 = ab(1);
% points D and E are the top 2 corners of the rectangle we are calculating
% Slopes of lines a->d and b->e is perpendicular to line ab, 
% so ad_1 = - 1/ab_1
ad_1 = -1/ab_1;
% line b-> is parallel to ac so they have the same slope
be_1 = ad_1;
% Intercept of a->c
ad_0 = a(y) - a(x)*ad_1;
be_0 = b(y) - b(x)*be_1;
% line d->e is parallel to to a->b and goes through point c
de_1 = ab_1;
de_0 = c(y) - c(x)*de_1;
% To work out the 4 corners of the rectangle of interest we can now
% calculate the points at which these lines intercept
% top left is the intersection of d->e and a->d [i.e. point d]
topleft(x) = (ad_0 - de_0)/(de_1 - ad_1); 
topleft(y) = ad_0 + ad_1*topleft(x);
% top right is the intersection of d->e and b->e [i.e. point d]
topright(x) = (be_0 - de_0)/(de_1 - ad_1); 
topright(y) = be_0 + be_1*topright(x);

bottomleft = a;
bottomright = b;

% % Optional plot fo points to verify it looks correct
% plot([bottomleft(x), bottomright(x)], [bottomleft(y), bottomright(y)])
% hold on
% plot([topright(x),topleft(x)], [topright(y),topleft(y)])
% plot(c(x), c(y), 'ro')
% text(bottomleft(x), bottomleft(y), 'bottomleft')
% text(bottomright(x), bottomright(y), 'bottomright')
% text(topleft(x), topleft(y), 'topleft')
% text(topright(x), topright(y), 'topright')
% hold off

end
