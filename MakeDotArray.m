function [dot_centers, dd] = MakeDotArray(side,number_dots,dot_diameter,dist)
% BASE CODE AND IDEA: Elon Gaffin-Cahn (December 2013). 
% ADDED: dot_diameter can be a vector i.e. to paint dots with different 
%        sizes. Santiago Alonso-Diaz (Aug. 2016) 

% Finds the centers of a set of dots within a larger allowed circle. It
% works by manipulating a logic matrix, with 1s meaning allowed positions.
% As dots are placed they 'erase' 1s by converting them to 0s. It places 
% dots in randoms centers by means of a sample from a multinomial 
% distribution in which each allowed position is a possible outcome with 
% equal probability.
 
% Input arguments:
%   SIDE:  diameter of allowed circle where the dots can appear (in pixels)
%   NUMBER_DOTS: number of dots in the array
%   DOT_DIAMETER: diameter in pixels of each dot. It could be a scalar or a
%       row vector. If all dots have equal size, use a scalar is much faster.
%       If vector, then its length has to be equal to  number_dots i.e. 
%       for each dot please specify a diameter.
%   DIST: scalar (zero and pos int) with minimum distance between dots (in pixels). 
%       If zero, dots could touch but never overlap
% 
% Output argument:
%   DOT_CENTERS [NUMBER_DOTS x 2] matrix of (x,y) pixel coordinates of the
%       centers of the dots
%   DD: dot diameters for each of the elements in DOT_CENTERS


% Example of use. To run in command window or in a script
% side = 325; number_dots = 100; dist = 5;
% dot_diameters = randi([5,20],1,number_dots);
% [dc, dd] = MakeDotArray(side,number_dots,dot_diameters,dist);
% items_colored = randi([1 number_dots],1,20);
% colors = [0 1 0; 1 0 0]; %1st row base color; 2nd row color of selected items
% fig = figure('Position',[300, 200, 400, 400]);
% hold on
% for i = 1:number_dots
%     c = colors(1,:);
%     if (any(i == items_colored)) 
%         c = colors(2,:); 
%     end
%     
%     x = dc(i,1);
%     y = dc(i,2);
%     r = dd(i)/2;
%     pos = [x-r, y-r, 2*r, 2*r];
%     rectangle('Position',pos, 'Curvature',[1 1], 'FaceColor',c)
%     axis equal
% end
% axis off



% parameters
bounds = [round(side), round(side)]; % height and width of allowed area circle
area_center = bounds/2; % x,y coordinates of center of allowed area circle
check = size(dot_diameter);
if check(1) ~= 1
    error('dot_diameter is not a row vector')
end
dd = sort(dot_diameter,2,'descend'); %to position big diameters first

% initialize the circle where the dots area allowed (in LOGICAL format)
allowed_area = MakeCircle(side - (dd(1) + dist)/2, area_center, bounds);
% allowed_area_o=allowed_area; %for visualization

for i = 1:number_dots 
    if isscalar(dd) %same dot size
        if all(~allowed_area(:))
            error('Density of the dot array too high!')
        end
        
        % get the (x,y) center of the current dot (choose randomly)
        if side>round(dd+dist)
           center = find(mnrnd(1, allowed_area(:)/sum(allowed_area(:)))); %This generates a linear index (scalar). Note: mnrnd.m code is available from octave, in case the stats toolbox is unavailable (https://sourceforge.net/p/octave/statistics/ci/1b89caebeccd691748beb0cac0e15f4e057240ce/tree/inst/mnrnd.m)           
           [center(1), center(2)] = ind2sub(size(allowed_area),center); % convert the linear index to center coordinates (x,y)
        elseif side == round(dd+dist)
           center = area_center;
        else
            error('Density of the dot array too high!')
        end
        
        % further restrict the allowed area
        allowed_area = allowed_area & ~MakeCircle(2*(dd+dist),center,bounds); %2*diameter to make sure dots won't overlap
        
        % add to growing list of the (x,y) centers of the dots
        dot_centers(i,:) = center; %#ok<AGROW>
        
    else %different dot sizes
        
        if all(~allowed_area(:))
            error('Density of the dot array too high!')
        end
        
        
        % get the (x,y) center of the current dot (choose randomly)
        center = find(mnrnd(1, allowed_area(:)/sum(allowed_area(:))));
        
        % convert the linear index to center coordinates (x,y)
        [center(1), center(2)] = ind2sub(size(allowed_area),center);
        
        % add to growing list of the (x,y) centers of the dots
        dot_centers(i,:) = center; %#ok<AGROW>
        
        if i<number_dots
            %with different dot diameters, allowed_area has to be
            %restricted from scratch to place the next dot i.e. the 'halos'
            %due to the next diameter are different from the previous dot
            %(see make circle below).
            allowed_area = MakeCircle(side - (dd(i+1) + dist)/2, ...
                area_center, bounds);
%             allowed_area_o = allowed_area; %for visualization
            
            for j = 1:i
                % further restrict the allowed area
                centerT = dot_centers(j,:);
                diameterT = (dd(j) + dd(i+1) + 2*dist); %redraws all previous dots with new 'halo', based on the diameter of the next dot to place
                allowed_area = allowed_area & ~MakeCircle(diameterT,centerT,bounds);
            end
        end
    end
end

if isscalar(dd) 
    dd = dd*ones(number_dots,1);
end




function circle = MakeCircle(diameter,center,bounds)
% Arguments:
%   diameter: of circle or dot. Circle refers to the big circle  where dots
%             are placed 
%   center: (x,y) coordinates of circle or dot
%   bounds: width and height of allowed bounds for dots to appear in. 
%           It has to be larger or equal than diameter.

% For a [bounds x bounds] matrix, pixel2origin(n,m,:) is the (x,y) vector
% from the origin (0,0) to (n,m). Origin refers to the top left corner of
% the screen, and flipped Y coordinates as in PTB (also, all coordinates are
% positive due to meshgrid call) i.e. this is just all the possible
% coordinates/pixel combinations in the  box defined by bounds.
[pixel2origin(:,:,2), pixel2origin(:,:,1)] = meshgrid(1:bounds(2),1:bounds(1)); 


% origin2center is the (x,y) vector from the origin [0,0] to the circle
% center. Minus sign to compute vector substraction i.e. the resulting
% vector from the substraction connects the two vectors (see below)
origin2center = -center; 

% scroll through x, then y
for i = 1:2
    % center2pixel is the (x,y) vector from the center of the dot or circle
    % to the current pixel at (m,n) 
    center2pixel(:,:,i) = pixel2origin(:,:,i) + origin2center(i); %#ok<AGROW>
end

% convert vector to magnitude only
absolute_distance = sqrt(sum(center2pixel.^2,3));

% circle is a logical array where TRUEs are where the distances are less
% than the radius. 
circle = absolute_distance <= diameter/2; %NOTE: you could change this for other shapes


