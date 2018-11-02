function MarkerPos = FindPosition(orig_frame, new_frame, rat_thresh, min_pixels, hBlobRat, plotfigs, stelparam, LED, lastxy)
        % Tracking steps:
        % (1) Get red/green component of the image
        % (2) Median filter
        % (3) Make binary mask
        % (4) Get centroids of top 10 points, keep biggest
        % (5) Return annotated video
            % Process red stream
            
            diffFrameRat = imcomplement(new_frame); 
            %diffFrameRat = medfilt2(diffFrameRat, [3 3]); 
                        
% %             %apply low threshold to get only light
% %             temp_thresh = 0.1;
% %             %set brightest parts of image (blackest in complement) to black 
% %             binFrameLight = im2bw(diffFrameRat, temp_thresh); 
                        
            %look for rat
            binFrameRat = im2bw(diffFrameRat, rat_thresh); 
            
% %             %subtracking pixels detected in both binFrameLight and
% %             %binFrameRat
% %             binFrameRat(~binFrameLight) = 1;
% %             %subtract light
            
%-----------------og method--------------------------------------------%
% % %             [areaRat,centroidRat] = step(hBlobRat, ~binFrameRat); 
% % %             [~,index] = max(areaRat);  
% % %             centroidRat=centroidRat(index,:);
% % %             centroidRat = uint16(centroidRat); 
%----------------------------------------------------------------------%            
            
            [xy] = detect_rat(orig_frame, binFrameRat, plotfigs, stelparam, min_pixels, hBlobRat, LED, lastxy);
            centroidRat = [xy];
            if ~isempty(centroidRat)
                MarkerPos = centroidRat;
                
                if plotfigs
                    figure
                    imshow(new_frame)
                    hold on 
                    plot(centroidRat(1),centroidRat(2),'go')

                    figure
                    imshow(binFrameRat)
                    hold on 
                    plot(centroidRat(1),centroidRat(2),'go')
                end
            else
                MarkerPos = [NaN,NaN];
            end
            
            
end