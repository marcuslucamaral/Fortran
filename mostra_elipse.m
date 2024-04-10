filename = 'Dados_Elipse_ruido.dat';
headerlinesIn = 1;
delimiterIn = ' ';
d = importdata(filename,delimiterIn,headerlinesIn);
x = d.data(:,1);
y = d.data(:,2);

e = load('Elipse_MQ.dat');
x0 = e(1);
y0 = e(2);
b = e(3);
a = e(4);

t = (0: 0.01: 2*pi)';
x2 = x0 + b * cos(t);
y2 = y0 + a * sin(t);

figure
plot(x,y,'*',x2,y2,'r')
axis equal
grid

