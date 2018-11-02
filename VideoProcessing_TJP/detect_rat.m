function [xy] = detect_rat(og_frame, binFrameRat, plotfig, strel_param, min_pixels, hBlobRat, LED, lastxy)
    bw = bwareaopen(~binFrameRat,min_pixels);
    
    if plotfig
        imshow(binFrameRat)
        figure;imshow(bw)
    end
    se = strel('disk', strel_param);
    closeBW = imclose(bw,se);
    
    if plotfig
        figure;imshow(closeBW)
    end
    
    
    %keep only biggest centroid
    labeledImage = bwlabel(closeBW, 8);
    blobMeasurements = regionprops(labeledImage, closeBW, 'all');
    numberOfBlobs = size(blobMeasurements, 1);
    
    [areaRat,centroidRat] = step(hBlobRat, closeBW); 
    [rat_size,index] = max(areaRat);  
    centroidRat=centroidRat(index,:);
    centroidRat = uint16(centroidRat); 
    
    if ~isempty(centroidRat)
        %remove everything smaller than biggest
        bw3 = bwareaopen(closeBW,double(rat_size)-1);
    else
        xy = [];
        return
    end
    %how big roughlyis the rat, if shape is much smaller, expand the elipse
    
    
    BW2 = bwmorph(bw3,'remove');
    if plotfig
        h = figure;
        hold on
        imshow(closeBW)
         hold on
    end
    
    if LED
        xy = centroidRat;
    else      
        [r,c]=find(BW2);
        ellipse_t = fit_ellipse( c,r);

        %find which line is longest
        ver_size = pdist2(ellipse_t.ver_line(1:2),ellipse_t.ver_line(3:4));
        horz_size = pdist2(ellipse_t.horz_line(1:2),ellipse_t.horz_line(3:4));

        if ver_size > horz_size
            xy = ellipse_t.ver_line;            
        else
            xy = ellipse_t.horz_line;            
        end
        
        if plotfig
            plot(c,r,'.')

            set(gca,'Ydir','reverse')
            ylim([0, size(binFrameRat,1)]);
            xlim([0, size(binFrameRat,2)]);
            plot(xy(1), xy(2),'gx');
            plot(xy(3), xy(4),'rx');
                    
        end
        
        
        if isempty(lastxy)
            figure
            imshow(og_frame)
            hold on
            set(gca,'Ydir','reverse')
            ylim([0, size(binFrameRat,1)]);
            xlim([0, size(binFrameRat,2)]);
            plot(c,r,'.')
            plot(xy(1), xy(2),'go');
            plot(xy(3), xy(4),'ro');
        
            %heads or tails
            ButtonName = questdlg('Head green or red?', ...
                                 'Which is the animals head?', ...
                                 'Green', 'Red', 'Bad Cances');
            switch ButtonName,
             case 'Green',
              	xy = xy(1:2);
                plot(xy(1), xy(2),'bo');
             case 'Red',
              	xy = xy(3:4);
                plot(xy(3), xy(4),'bo');
             case 'Bad Cances',
                error('try debug mode and change thresholds');
            end % switch
        %we already have a head position take whichever is nearest
        else
            dist12 = pdist2(xy(1:2),lastxy);
            dist34 = pdist2(xy(3:4),lastxy);
            
            if abs(dist12) < abs(dist34)
                xy = xy(1:2);
            else
                xy = xy(3:4);
            end
        end
    
        
    end