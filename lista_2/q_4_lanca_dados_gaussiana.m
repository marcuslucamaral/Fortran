'coordenadas_ordenadas.dat'
data = load ('q_4_plot.dat')
x = data(:,1)
y = data(:,2)

%

plot(x,y);
grid
xlabel('Valor obtido no lançamento');
ylabel('Probabilidade em %');
title('Probabilidades no lançamento de n dados');
