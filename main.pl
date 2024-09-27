% Definição das colunas e linhas
coluna(a). coluna(b). coluna(c). coluna(d).
coluna(e). coluna(f). coluna(g). coluna(h).

linha(1). linha(2). linha(3). linha(4).
linha(5). linha(6). linha(7). linha(8).

% Mapeia colunas para números para cálculo da paridade
coluna_num(a, 1). coluna_num(b, 2). coluna_num(c, 3). coluna_num(d, 4).
coluna_num(e, 5). coluna_num(f, 6). coluna_num(g, 7). coluna_num(h, 8).

% Representação das peças
peça(jogador_a, peca_a).  % Jogador A é o computador
peça(jogador_b, peca_b).  % Jogador B é o jogador humano

% Definição das linhas para impressão (de 8 a 1)
linha_impressao(L) :- between(1, 8, N), L is 9 - N.

% Determina a cor da casa (somente casas escuras são válidas)
cor_casa(Col, Lin, escura) :-
    coluna_num(Col, NumCol),
    Paridade is (NumCol + Lin) mod 2,
    Paridade =:= 0.

% Posicionamento inicial das peças no tabuleiro
inicializa_tabuleiro(Tabuleiro) :-
    findall((Col, Lin, Peca), posicao_inicial(Col, Lin, Peca), Tabuleiro).

% Definição das posições iniciais das peças
posicao_inicial(Col, Lin, Peca) :-
    linha(Lin), coluna(Col),
    cor_casa(Col, Lin, escura),
    (
        (Lin >= 1, Lin =< 3, peça(jogador_a, Peca));  % Peças do computador
        (Lin >= 6, Lin =< 8, peça(jogador_b, Peca));  % Peças do jogador humano
        (Lin >= 4, Lin =< 5, Peca = vazio)
    ).
posicao_inicial(Col, Lin, Peca) :-
    linha(Lin), coluna(Col),
    \+ cor_casa(Col, Lin, escura),
    Peca = vazio.

% Imprime o tabuleiro
imprime_tabuleiro(Tabuleiro) :-
    nl,
    write('  A B C D E F G H'), nl,
    linha_impressao(Lin),
    write(Lin), write(' '),
    imprime_linha(Tabuleiro, Lin),
    fail.
imprime_tabuleiro(_) :- nl.

% Imprime cada linha do tabuleiro
imprime_linha(Tabuleiro, Lin) :-
    coluna(Col),
    ( member((Col, Lin, Peca), Tabuleiro) ->
        simbolo_peca(Peca, Simbolo);
        Simbolo = ' '
    ),
    write(Simbolo), write(' '),
    (Col == h -> nl ; true),
    fail.
imprime_linha(_, _) :- true.

% Define os símbolos para cada peça
simbolo_peca(peca_a, 'A').  % Computador
simbolo_peca(peca_b, 'B').  % Jogador humano
simbolo_peca(vazio, '.').
simbolo_peca(_, ' ').

% Inicia o jogo
iniciar :-
    inicializa_tabuleiro(TabuleiroInicial),
    jogar(TabuleiroInicial, jogador_b).  % Jogador humano começa

% Alterna o jogador
prox_jogador(jogador_a, jogador_b).
prox_jogador(jogador_b, jogador_a).

% Loop principal do jogo
jogar(Tabuleiro, JogadorAtual) :-
    imprime_tabuleiro(Tabuleiro),
    ( jogo_terminado(Tabuleiro, Vencedor) ->
        format('Jogo terminado! O vencedor é: ~w~n', [Vencedor]);
        (JogadorAtual == jogador_a ->
            jogador_programa(Tabuleiro, NovoTabuleiro, JogadorAtual);
            jogador_humano(Tabuleiro, NovoTabuleiro, JogadorAtual)),
        prox_jogador(JogadorAtual, ProxJogador),
        jogar(NovoTabuleiro, ProxJogador)
    ).

% Função para o jogador humano fazer uma jogada
jogador_humano(Tabuleiro, NovoTabuleiro, Jogador) :-
    format('Sua vez, Jogador ~w.~n', [Jogador]),
    write('Digite a coluna da peça que deseja mover (a-h): '), read(ColOrig),
    write('Digite a linha da peça que deseja mover (1-8): '), read(LinOrig),
    write('Digite a coluna de destino (a-h): '), read(ColDest),
    write('Digite a linha de destino (1-8): '), read(LinDest),
    (move_valido(Tabuleiro, Jogador, (ColOrig, LinOrig), (ColDest, LinDest)) ->
        atualiza_tabuleiro(Tabuleiro, (ColOrig, LinOrig), (ColDest, LinDest), Jogador, NovoTabuleiro);
        write('Movimento inválido! Tente novamente.'), nl,
        jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
    ).

% Função para o computador fazer uma jogada (movimento aleatório válido)
jogador_programa(Tabuleiro, NovoTabuleiro, Jogador) :-
    write('Vez do computador...'), nl,
    findall((ColO, LinO, ColD, LinD),
            move_possivel(Tabuleiro, Jogador, (ColO, LinO), (ColD, LinD)),
            Movimentos),
    (Movimentos \= [] ->
        random_member((ColOrig, LinOrig, ColDest, LinDest), Movimentos),
        format('Computador move de (~w,~w) para (~w,~w).~n', [ColOrig, LinOrig, ColDest, LinDest]),
        atualiza_tabuleiro(Tabuleiro, (ColOrig, LinOrig), (ColDest, LinDest), Jogador, NovoTabuleiro);
        NovoTabuleiro = Tabuleiro,
        write('Computador não tem movimentos válidos.'), nl
    ).

% Verifica se o movimento é válido
move_valido(Tabuleiro, Jogador, (ColO, LinO), (ColD, LinD)) :-
    move_possivel(Tabuleiro, Jogador, (ColO, LinO), (ColD, LinD)).

% Gera todos os movimentos possíveis para um jogador
move_possivel(Tabuleiro, Jogador, (ColO, LinO), (ColD, LinD)) :-
    member((ColO, LinO, Peca), Tabuleiro),
    peça(Jogador, Peca),
    movimento_valido(Jogador, (ColO, LinO), (ColD, LinD)),
    casa_vazia(Tabuleiro, (ColD, LinD)).

% Verifica se a casa de destino está vazia
casa_vazia(Tabuleiro, (Col, Lin)) :-
    member((Col, Lin, Peca), Tabuleiro),
    Peca == vazio.

% Define movimentos válidos para cada jogador
movimento_valido(jogador_a, (ColO, LinO), (ColD, LinD)) :-  % Computador
    coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
    LinD is LinO + 1,
    (NumColD is NumColO -1 ; NumColD is NumColO + 1),
    linha(LinD), coluna(ColD),
    cor_casa(ColD, LinD, escura).

movimento_valido(jogador_b, (ColO, LinO), (ColD, LinD)) :-  % Jogador humano
    coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
    LinD is LinO - 1,
    (NumColD is NumColO -1 ; NumColD is NumColO + 1),
    linha(LinD), coluna(ColD),
    cor_casa(ColD, LinD, escura).

% Atualiza o tabuleiro após um movimento
atualiza_tabuleiro(Tabuleiro, (ColO, LinO), (ColD, LinD), Jogador, NovoTabuleiro) :-
    select((ColO, LinO, Peca), Tabuleiro, TempTab1),
    peça(Jogador, Peca),
    select((ColD, LinD, vazio), TempTab1, TempTab2),
    NovoTabuleiro = [(ColD, LinD, Peca), (ColO, LinO, vazio)|TempTab2].

% Verifica se o jogo terminou
jogo_terminado(Tabuleiro, Vencedor) :-
    ( \+ member((_, _, peca_a), Tabuleiro) ->
        Vencedor = 'Jogador Humano' ;
      \+ member((_, _, peca_b), Tabuleiro) ->
        Vencedor = 'Computador' ;
      false
    ).
