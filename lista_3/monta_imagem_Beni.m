clear all;

ny = 1152;
nx = 2048;
%FF = zeros(ny,nx,3);
FF2= zeros(nx/2,ny/2,3);

%FF(:,:,1) = load('Red_beni.dat');
%FF(:,:,2) = load('Green_beni.dat');
%FF(:,:,3) = load('Blue_beni.dat');

FF2(:,:,1) = load('Red2_beni.dat');
FF2(:,:,2) = load('Green2_beni.dat');
FF2(:,:,3) = load('Blue2_beni.dat');

%figure('color','k')
%image(FF)
%axis image

figure('color','k')
image(FF2)
axis image

