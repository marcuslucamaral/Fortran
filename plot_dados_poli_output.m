clear all;

data = load('dados_poli_output.dat');  % Carrega os dados do arquivo

x = data(:, 1);  % Valores de x (primeira coluna)
y_noise = data(:, 2);  % Valores de y com ruído (segunda coluna)
y_real = data(:, 3);  % Valores de y real (terceira coluna)

figure;  % Cria uma nova figura

% Plota os pontos de y com ruído como uma "bola"
scatter(x, y_noise, 'filled', 'MarkerFaceColor', 'b');

hold on;  % Mantém a figura atual para adicionar mais elementos

% Plota os pontos de y real como uma linha contínua
plot(x, y_real, 'r-');

% Configurações adicionais do gráfico
xlabel('x');
ylabel('y');
legend('y com ruído', 'y real');
title('Gráfico com y com ruído e y real');
grid on;

hold off;  % Libera a figura para edição futura (caso necessário)
