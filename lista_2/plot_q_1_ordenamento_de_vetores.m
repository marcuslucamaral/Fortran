'coordenadas_ordenadas.dat'
data = load ('coordenadas_ordenadas.dat')
x = data(:,2)
y = data(:,3)

%

plot(x,y);
grid
xlabel('x');
ylabel('y');
title('function y sin(x)');
