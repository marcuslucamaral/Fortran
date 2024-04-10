
data = load ('q_6_plot_h_1.dat')
x = data(:,1);
Ud = data(:,2);
Ub = data(:,3);
%

plot(x,Ud,'b');
hold on
plot(x,Ub,'r');

grid

xlabel('Coordenadas');
ylabel('Potencial');
title('Function Potencial com H=1(coordenadas x)');
legend('Potencial Dipolo em preto', 'Potencial Bipolo em vermelho')

