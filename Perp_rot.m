function [ N_rot_1, N_rot_2 ] = Perp_rot( N, V, L )
%Santiago Alonso-Diaz (2016)
%Perpendicular rotation of a normal vector of a 3D plane so that it lies 
%on the plane surface. The rotation is further constrained to be 
%perpendicular to another vector on the plane (i.e otherwise there are 
%infinitely many).  
%
%One way such rotation is useful is when trying to locate points that live
%in 2D space (say pixels in computer screen) and find their corresponding 
%position in 3D space (say a room). See 2D_pixels_to_3Dmm.m for an actual 
%code on this application. 

% Formally, this code is the solution to this:
%         N.N_rot = 0; V.N_rot = 0; norm(N_rot) = L;
%N_rot is unknown and . stands for dot product.

% Inputs
    % N: normal vector to plane (the one to be rotated)
    % V: vector on the plane that rotated vector is perpendicular to
    % L: scalar with desired length of rotated vector
% Output
    % Two vectors N_rot_1 and N_rot_2, each one a possible perpendicular 
    %rotation e.g. left and right. It dependes on V.


k = N(1)*V(2) - N(2)*V(1); 
a = ((N(1)*N(2)*V(3))^2 - 2*N(1)*N(2)^2*N(3)*V(1)*V(3) + ...
    (N(2)*N(3)*V(1))^2 - 2*N(1)*N(2)*N(3)*V(3)*k + 2*N(2)*N(3)^2*V(1)*k + ...
    (k*N(3))^2)/((k*N(1))^2) +((N(3)*V(1))^2 - 2*N(1)*N(3)*V(1)*V(3) +  ... 
    (N(1)*V(3))^2)/(k^2) + 1; 
b = 0;
c = -(L^2);  
z = [(-b + sqrt(b^2 - 4*a*c))/(2*a), (-b - sqrt(b^2 - 4*a*c))/(2*a)]; %quadratic formula
y = [(V(1)*N(3)*z(1) - V(3)*N(1)*z(1))/(V(2)*N(1) - V(1)*N(2)), ...
    (V(1)*N(3)*z(2) - V(3)*N(1)*z(2))/(V(2)*N(1) - V(1)*N(2))];
x = [(-N(2)*y(1) - N(3)*z(1))/N(1), ...
    (-N(2)*y(2) - N(3)*z(2))/N(1)];

N_rot_1 = [x(1), y(1), z(1)]; 
N_rot_2 = [x(2), y(2), z(2)]; 



end

