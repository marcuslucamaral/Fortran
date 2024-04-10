Grav = load('Gravimetri2Sphere.dat');
%surf(Grav)
mesh(Grav)
shading interp;
colormap(cool(10))
%colorbar;