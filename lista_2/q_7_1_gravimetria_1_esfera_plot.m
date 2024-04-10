data = load ('q_7_1_gravimetria_1_esfera.dat')
x = data(:,1);
g = data(:,2);

%

plot(x,g,'b');

grid

xlabel('x(m)');
ylabel('delta g (miliGal)');
title('Levantamento Gravimetrico');
legend('Uma esfera:delta d =2g/cm^3, R = 20 m, z_0= 25m')

