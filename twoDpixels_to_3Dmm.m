function [cPoints_3D_mm ] = twoDpixels_to_3Dmm(pos_stim, rp, monitor)
%Santiago Alonso-Diaz (2016)
%This function will transform 2D pixel coordinates to 3D coordinates(mm).
%
%You may want to do this to make figures in a paper and show where in 3D
%space the stimuli was located.
%
%Input
%
%   pos_stim:       matrix n x 2 with x,y center of the stimuli in pixels.
%                   each row is a different stimulus
%   rp:             matrix 3x3. Three reference points on the monitor in 
%                   3D space (e.g. obtainable with an infrared recording 
%                   device such as an optotrak). 
%                   Row 1: top right corner of the monitor (x, y, z)(mm)
%                   Row 2: top left corner of the monitor
%                   Row 3: center of the monitor
%   monitor:        matrix 2 x 2: Row 1 display dimensions in mm (width, height)
%                   Row 2 display resolution in pixels (width, height)



center_screen = rp(3,:);
px_mm = monitor(2,:)./monitor(1,:);
vT = rp(1,:) - rp(2,:); %top monitor vector i.e. from top left to top right marker
vA = center_screen - rp(1,:);
vB = center_screen - rp(2,:);
N = cross(vA,vB);
N = N/norm(N); %unit normal vector to the plane
%Rotation of the normal vector so that it is perpendicular to the vector
%determined by the two vector delimiting the monitor
[ N_rot_1_vert, ... bottom rotation
    N_rot_2_vert ... top rotation
    ] = Perp_rot( N, vT, monitor(1,2)/2 );
V = N_rot_1_vert/norm(N_rot_1_vert);
%Rotation of the normal vector so that it is perpendicular to the previous
%vector
vV = N_rot_2_vert - N_rot_1_vert;
[ N_rot_1_horz, ... right rotation
    ~ ... left rotation
    ] = Perp_rot( N, vV, monitor(1,1)/2 );
H = N_rot_1_horz/norm(N_rot_1_horz);

tempH = ((pos_stim(:,1) - monitor(2,1)/2)./px_mm(1)); %Position relative to the center
tempV = ((pos_stim(:,2) - monitor(2,2)/2)./px_mm(2));
cPoints_3D_mm = zeros(size(pos_stim,1),3); %coordinate of stimulus in 3D (mm) (x,y,z)
for i = 1:size(pos_stim,1)
    cPoints_3D_mm(i,:) = H*tempH(i) + V*tempV(i) + center_screen;
end

end