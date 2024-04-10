[x,y,z] = textread ("solucao.dat");

scatter(x,y, 15,"filled")
hold on
plot(x,z,'k')
xlabel ('x values');
ylabel ('y values');
title ('Dados com ruído Aleatório x Ajuste');
grid on
