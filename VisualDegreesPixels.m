function [ conversion ] = VisualDegreesPixels( mDim, d, mRes, size, type)
%Converts pixels to visual degrees or pixels to visual degrees

%%%Inputs
    %mDim:     Monitor dimensions [width, height] (cm)
    %d:        Distance between monitor and participant (cm)
    %mRes:     Monitor resolution [width, height] (pixels)
    %size:     stimulus size in pixels or degrees [width, height] (IMPORTANT: it has to match 'type')
    %type:     type of conversion; 1: pixels to degrees; 2: degrees to pixels

%%%Outputs
    %conversion: number of pixels or degrees (depends on 'type' of conversion) [width, height] 


if type == 1 %Pixels to degrees
    rad = atan((size(1)/2)/(d*mRes(1)/mDim(1)));
    visangle_width = rad*(180/pi)*2;
    
    rad = atan((size(2)/2)/(d*mRes(2)/mDim(2)));
    visangle_height = rad*(180/pi)*2;  

    conversion  = [visangle_width, visangle_height];
    
elseif type == 2 %Degrees to pixels
    deg_to_rad = (size(1)/2)*(pi/180);
    sizeInPixels_width = tan(deg_to_rad)*2*d*mRes(1)/mDim(1);
    
    deg_to_rad = (size(2)/2)*(pi/180);
    sizeInPixels_height = tan(deg_to_rad)*2*d*mRes(2)/mDim(2);
    
    conversion  = [sizeInPixels_width, sizeInPixels_height];
end

end

