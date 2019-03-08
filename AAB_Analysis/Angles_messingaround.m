% Calculate Head angle: Nose angle relative to ears
% angle between two vectors : cos(?)= (v?w)/(?v??w?)


% Step 1: calculate the mid point between the ears
midpoint = [mean([REar(:,1),LEar(:,1)],2), mean([REar(:,2),LEar(:,2)],2)];

% Take this as the centre point of the co-ordinates by subtracting from them
nose_cent = nose-tail;
LEar_cent = LEar-tail;
REar_cent = REar-tail;
nose_point = nose-midpoint;
midpoint_cent = midpoint-tail;

% Find the angle (degrees) between two vectors
nose_LEar_angle = acosd(dot(nose_cent,LEar_cent)/(norm(nose_cent)*(norm(LEar_cent))))

scatter(nose(1), nose(2))
hold on; 
scatter(LEar(1), LEar(2))
scatter(REar(1), REar(2))
scatter(mean([REar(1),LEar(1)]), mean([REar(2),LEar(2)]))
legend('Nose', 'Left ear', 'Right ear', 'Centre')
hold off

figure
scatter(nose_cent(1), nose_cent(2))
hold on; 
scatter(LEar_cent(1), LEar_cent(2))
scatter(REar_cent(1), REar_cent(2))
scatter(0,0)
legend('Nose', 'Left ear', 'Right ear', 'Centre')
hold off

plot(unwrap(atan2(diff(nose_cent(:,1)), diff(nose_cent(:,2)))))




%in radians
signedangle = atan2(Y,X);
%deals with discontinuities in the polar co-ordinates
unwrap(signedangle)
diff(unwrap(atan2((nose_cent(:,1)), (nose_cent(:,2)))))
atan2((nose_cent(:,1)), (nose_cent(:,2)))

gridy = zeros(max(ceil(X)),max(ceil(X)));
for i = 1:size(X,1)
    x = ceil(X(i));
    y  = ceil(Y(i));
   gridy(x,y) = gridy(x,y) +1;
end
