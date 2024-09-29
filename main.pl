% Definição das colunas e linhas
coluna(a). coluna(b). coluna(c). coluna(d).
coluna(e). coluna(f). coluna(g). coluna(h).

linha(1). linha(2). linha(3). linha(4).
linha(5). linha(6). linha(7). linha(8).

% Mapeia colunas para números para cálculo da paridade
coluna_num(a, 1). coluna_num(b, 2). coluna_num(c, 3).
coluna_num(d, 4). coluna_num(e, 5). coluna_num(f, 6).
coluna_num(g, 7). coluna_num(h, 8).

% Representação das peças
peça(jogador_a, peca_a).
peça(jogador_b, peca_b).

% Representação das damas
dama(jogador_a, dama_a).
dama(jogador_b, dama_b).

% Definição das linhas para impressão (de 8 a 1)
linha_impressao(L) :- between(1, 8, N), L is 9 - N.

% Determina a cor da casa
cor_casa(Col, Lin, escura) :-
    coluna_num(Col, NumCol),
    Paridade is (NumCol + Lin) mod 2,
    Paridade =:= 0.

% Posicionamento inicial das peças no tabuleiro
inicializa_tabuleiro(Tabuleiro) :-
    findall((Col, Lin, Peca), posicao_inicial(Col, Lin, Peca),
            Tabuleiro).

% Definição das posições iniciais das peças
posicao_inicial(Col, Lin, Peca) :-
    linha(Lin), coluna(Col), cor_casa(Col, Lin, escura),
    (
        (Lin >= 1, Lin =< 3, peça(jogador_a, Peca));
        (Lin >= 6, Lin =< 8, peça(jogador_b, Peca));
        (Lin >= 4, Lin =< 5, Peca = vazio)
    ).
posicao_inicial(Col, Lin, Peca) :-
    linha(Lin), coluna(Col), \+ cor_casa(Col, Lin, escura),
    Peca = vazio.

% Imprime o tabuleiro
imprime_tabuleiro(Tabuleiro) :-
    nl, format('   A   B   C   D   E   F   G   H~n'),
    linha_impressao(Lin),
    format('~w ', [Lin]), imprime_linha(Tabuleiro, Lin), nl,
    fail.
imprime_tabuleiro(_) :- nl.

% Imprime cada linha do tabuleiro
imprime_linha(Tabuleiro, Lin) :-
    coluna(Col),
    ( member((Col, Lin, Peca), Tabuleiro) ->
        simbolo_peca(Peca, Simbolo);
        Simbolo = ' '
    ),
    format(' ~w', [Simbolo]),
    fail.
imprime_linha(_, _) :- true.

% Define os símbolos para cada peça e dama
simbolo_peca(peca_a, 'A').
simbolo_peca(peca_b, 'B').
simbolo_peca(dama_a, 'CA').
simbolo_peca(dama_b, 'CB').
simbolo_peca(vazio, '.').
simbolo_peca(_, ' ').

% Inicia o jogo
iniciar :-
    menu_principal(Opcao),
    inicializa_tabuleiro(TabuleiroInicial),
    (Opcao == 1 -> jogar(TabuleiroInicial, jogador_a);
     Opcao == 2 -> jogar(TabuleiroInicial, jogador_b);
     Opcao == 3 -> partida_automatica(TabuleiroInicial, jogador_a);
     Opcao == 4 -> partida_automatica(TabuleiroInicial, jogador_b)
    ).

% Exibe o menu principal
menu_principal(Opcao) :-
    repeat,
    write('Selecione uma opção:'), nl,
    write('1. Jogar contra o programa (Computador começa)'), nl,
    write('2. Jogar contra o programa (Você começa)'), nl,
    write('3. Assistir a uma partida automática (Jogador A começa)'), nl,
    write('4. Assistir a uma partida automática (Jogador B começa)'), nl,
    read(Opcao),
    ( member(Opcao, [1, 2, 3, 4]) -> !;
      write('Opção inválida!'), nl, fail
    ).

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

% Jogada do jogador humano
jogador_humano(Tabuleiro, NovoTabuleiro, Jogador) :-
    format('Vez do Jogador B ~n'),
    write('Digite mv(Coord1, Coord2) ou cap(Coord1, [Lista]): '),
    read_line_to_string(user_input, Input),
    (   catch(term_string(Movimento, Input), _, fail) ->
        processa_movimento(Movimento, Tabuleiro, NovoTabuleiro, Jogador)
    ;   write('Comando inválido!'), nl,
        jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
    ).

% Processa o movimento do jogador
processa_movimento(Movimento, Tabuleiro, NovoTabuleiro, Jogador) :-
    (Movimento = mv(PosOrig, PosDest) ->
        (validar_coordenadas(PosOrig), validar_coordenadas(PosDest) ->
            atom_chars(PosOrig, [ColOrig, LinOrigChar]),
            atom_number(LinOrigChar, LinOrig),
            atom_chars(PosDest, [ColDest, LinDestChar]),
            atom_number(LinDestChar, LinDest),
            (move_valido(Tabuleiro, Jogador, (ColOrig, LinOrig),
                         (ColDest, LinDest)) ->
                atualiza_tabuleiro(Tabuleiro, (ColOrig, LinOrig),
                                   (ColDest, LinDest), Jogador, NovoTabuleiro)
            ;
                write('Movimento inválido!'), nl,
                jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
            )
        ;
            write('Coordenadas inválidas!'), nl,
            jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
        )
    ; Movimento = cap(PosOrig, CapturaLista) ->
        (validar_coordenadas(PosOrig) ->
            atom_chars(PosOrig, [ColOrig, LinOrigChar]),
            atom_number(LinOrigChar, LinOrig),
            findall(MaiorCaptura,
                    maior_captura(Tabuleiro, Jogador, (ColOrig, LinOrig),
                                  MaiorCaptura), TodasCapturas),
            maior_caminho(TodasCapturas, CaminhoMaior),
            (validar_captura(Tabuleiro, Jogador, (ColOrig, LinOrig),
                             CapturaLista) ->
                length(CapturaLista, TamCaptura),
                length(CaminhoMaior, TamMaior),
                (TamCaptura < TamMaior ->
                    format('Captura inválida! Deve ser: cap(~w, ~w).~n',
                           [PosOrig, CaminhoMaior]),
                    jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
                ;
                    captura(Tabuleiro, Jogador, (ColOrig, LinOrig),
                            CapturaLista, NovoTabuleiro)
                )
            ;
                format('Captura inválida! Deve ser: cap(~w, ~w).~n',
                       [PosOrig, CaminhoMaior]),
                jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
            )
        ;
            write('Coordenadas inválidas!'), nl,
            jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
        )
    ;
        write('Comando inválido!'), nl,
        jogador_humano(Tabuleiro, NovoTabuleiro, Jogador)
    ).

% Valida as coordenadas
validar_coordenadas(Pos) :-
    atom_chars(Pos, [Col, LinChar]),
    member(Col, [a, b, c, d, e, f, g, h]),
    atom_number(LinChar, Lin),
    between(1, 8, Lin).

% Encontra o maior caminho de captura
maior_caminho([Caminho], Caminho).
maior_caminho([C1, C2 | R], Maior) :-
    length(C1, T1), length(C2, T2),
    (T1 >= T2 -> maior_caminho([C1 | R], Maior);
                 maior_caminho([C2 | R], Maior)).

% Valida a captura antes de executá-la
validar_captura(Tab, Jog, (ColO, LinO), CaptLista) :-
    normalizar_lista(CaptLista, CaptListaNormalizada),
    validar_captura_aux(Tab, Jog, (ColO, LinO), CaptListaNormalizada, _).

% Converte átomos para minúsculas
normalizar_lista([], []).
normalizar_lista([H|T], [HLower|TLower]) :-
    downcase_atom(H, HLower),
    normalizar_lista(T, TLower).


% Função auxiliar para validar captura
validar_captura_aux(Tab, Jog, (ColO, LinO), CaptLista, NovoTab) :-
    CaptLista = [PosD | Capt], % Garantir que a lista seja decomposta corretamente
    atom_chars(PosD, [ColD, LinDChar]),
    atom_number(LinDChar, LinD),
    (   member((ColO, LinO, Peca), Tab),
        (dama(Jog, Peca) ->
            mov_cap_valido_dama(Tab, Jog, (ColO, LinO), (ColD, LinD),
                                PosCapturadas),
            remover_pecas(Tab, PosCapturadas, TabSemCapt),
            casa_vazia(TabSemCapt, (ColD, LinD))
        ;
            mov_cap_valido(Tab, Jog, (ColO, LinO), (ColD, LinD))
        )
    ->  
        atualiza_tabuleiro_captura(Tab, (ColO, LinO), (ColD, LinD), Jog,
                                   NovoTabTemp),
        (Capt = [] -> NovoTab = NovoTabTemp;
         validar_captura_aux(NovoTabTemp, Jog, (ColD, LinD), Capt,
                             NovoTab))
    ;
        format('Captura inválida! Deve ser: cap(~w, ~w).~n',
               [ColO, CaptLista]), nl, fail
    ).

% Realiza a captura
captura(Tab, Jog, (ColO, LinO), CaptLista, NovoTab) :-
    captura_aux(Tab, Jog, (ColO, LinO), CaptLista, NovoTab).

captura_aux(Tab, Jog, (ColO, LinO), [PosD | Capt], NovoTab) :-
    atom_chars(PosD, [ColD, LinDChar]),
    atom_number(LinDChar, LinD),
    member((ColO, LinO, Peca), Tab),
    (dama(Jog, Peca) ->
        mov_cap_valido_dama(Tab, Jog, (ColO, LinO), (ColD, LinD),
                            PosCapturadas),
        remover_pecas(Tab, PosCapturadas, TabSemCapt),
        select((ColO, LinO, Peca), TabSemCapt, TempTab1),
        NovoTabTemp = [(ColD, LinD, Peca), (ColO, LinO, vazio)|TempTab1]
    ;
        mov_cap_valido(Tab, Jog, (ColO, LinO), (ColD, LinD)),
        coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
        LinM is (LinO + LinD) // 2, NumColM is (NumColO + NumColD) // 2,
        coluna_num(ColM, NumColM),
        select((ColO, LinO, Peca), Tab, TempTab1),
        select((ColM, LinM, PecaAdv), TempTab1, TempTab2),
        peca_adversaria(Jog, PecaAdv),
        select((ColD, LinD, vazio), TempTab2, TempTab3),
        NovoTabTemp = [(ColD, LinD, Peca), (ColO, LinO, vazio),
                       (ColM, LinM, vazio)|TempTab3]
    ),
    promover_se_necessario(NovoTabTemp, Jog, (ColD, LinD), NovoTabProm),
    (Capt = [] -> NovoTab = NovoTabProm;
     captura_aux(NovoTabProm, Jog, (ColD, LinD), Capt, NovoTab)).

% Remove múltiplas peças
remover_pecas(Tab, Posicoes, NovoTab) :-
    foldl(remover_peca, Posicoes, Tab, NovoTab).

remover_peca((Col, Lin), Tab, NovoTab) :-
    select((Col, Lin, _), Tab, [(Col, Lin, vazio)|NovoTab]).

% Verifica se uma peça é adversária
peca_adversaria(jogador_a, P) :- (peça(jogador_b, P); dama(jogador_b, P)).
peca_adversaria(jogador_b, P) :- (peça(jogador_a, P); dama(jogador_a, P)).

% Encontra a maior sequência de capturas
maior_captura(Tab, Jog, (ColO, LinO), MaiorCapt) :-
    findall(Captura, caminho_captura(Tab, Jog, (ColO, LinO), Captura),
            TodasCapturas),
    (TodasCapturas \= [] ->
        maior_caminho(TodasCapturas, MaiorCapt);
        MaiorCapt = []
    ).

% Encontra todas as capturas possíveis
caminho_captura(Tab, Jog, (ColO, LinO), [PosD | Capt]) :-
    member((ColO, LinO, Peca), Tab),
    (dama(Jog, Peca) ->
        mov_cap_valido_dama(Tab, Jog, (ColO, LinO), (ColD, LinD),
                            PosCapturadas),
        remover_pecas(Tab, PosCapturadas, TabSemCapt),
        select((ColO, LinO, Peca), TabSemCapt, TempTab1),
        NovoTab = [(ColD, LinD, Peca), (ColO, LinO, vazio)|TempTab1]
    ;
        mov_cap_valido(Tab, Jog, (ColO, LinO), (ColD, LinD)),
        coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
        LinM is (LinO + LinD) // 2, NumColM is (NumColO + NumColD) // 2,
        coluna_num(ColM, NumColM),
        select((ColO, LinO, Peca), Tab, TempTab1),
        select((ColM, LinM, PecaAdv), TempTab1, TempTab2),
        peca_adversaria(Jog, PecaAdv),
        select((ColD, LinD, vazio), TempTab2, TempTab3),
        NovoTab = [(ColD, LinD, Peca), (ColO, LinO, vazio),
                   (ColM, LinM, vazio)|TempTab3]
    ),
    atom_number(LinDAtom, LinD), atom_concat(ColD, LinDAtom, PosD),
    (caminho_captura(NovoTab, Jog, (ColD, LinD), Capt) -> true;
     Capt = []).

% Movimento de captura válido para a dama
mov_cap_valido_dama(Tab, Jog, (ColO, LinO), (ColD, LinD), PosCapt) :-
    nonvar(ColO), nonvar(LinO), nonvar(ColD), nonvar(LinD),
    coluna_num(ColO, NumColO),
    coluna_num(ColD, NumColD),
    DifCol is NumColD - NumColO,
    DifLin is LinD - LinO,
    abs(DifCol, AbsDifCol),
    abs(DifLin, AbsDifLin),
    AbsDifCol =:= AbsDifLin,
    DirecaoCol is sign(DifCol),
    DirecaoLin is sign(DifLin),
    caminho_dama_captura(Tab, Jog, (NumColO, LinO), (NumColD, LinD), DirecaoCol, DirecaoLin, false, PosCapt).


% Verifica o caminho da dama durante a captura
caminho_dama_captura(_, _, (NumColD, LinD), (NumColD, LinD), _, _, true, []) :- !.
caminho_dama_captura(Tab, Jog, (NumColA, LinA), (NumColD, LinD), DirCol, DirLin, Encontrou, PosCapt) :-
    NumColP is NumColA + DirCol,
    LinP is LinA + DirLin,
    nonvar(NumColP), nonvar(LinP),
    coluna_num(ColP, NumColP),
    linha(LinP),
    ( member((ColP, LinP, vazio), Tab) ->
        caminho_dama_captura(Tab, Jog, (NumColP, LinP), (NumColD, LinD), DirCol, DirLin, Encontrou, PosCapt)
    ; member((ColP, LinP, Peca), Tab),
      peca_adversaria(Jog, Peca), \+ Encontrou ->
        caminho_dama_captura(Tab, Jog, (NumColP, LinP), (NumColD, LinD), DirCol, DirLin, true, PosResto),
        PosCapt = [(ColP, LinP)|PosResto]
    ; Encontrou, member((ColP, LinP, vazio), Tab) ->
        caminho_dama_captura(Tab, Jog, (NumColP, LinP), (NumColD, LinD), DirCol, DirLin, true, PosCapt)
    ; fail
    ).
% Movimento de captura válido para a peça normal
mov_cap_valido(Tab, Jog, (ColO, LinO), (ColD, LinD)) :-
    coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
    (LinD is LinO + 2; LinD is LinO - 2),
    (NumColD is NumColO + 2; NumColD is NumColO - 2),
    LinM is (LinO + LinD) // 2, NumColM is (NumColO + NumColD) // 2,
    coluna_num(ColM, NumColM),
    member((ColM, LinM, PecaAdv), Tab),
    peca_adversaria(Jog, PecaAdv),
    casa_vazia(Tab, (ColD, LinD)).

% Atualiza o tabuleiro após uma captura
atualiza_tabuleiro_captura(Tab, (ColO, LinO), (ColD, LinD), Jog, NovoTab) :-
    select((ColO, LinO, Peca), Tab, TempTab1),
    (dama(Jog, Peca) ->
        select((ColD, LinD, vazio), TempTab1, TempTab2),
        NovoTabTemp = [(ColD, LinD, Peca), (ColO, LinO, vazio)|TempTab2]
    ;
        coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
        LinM is (LinO + LinD) // 2, NumColM is (NumColO + NumColD) // 2,
        coluna_num(ColM, NumColM),
        select((ColM, LinM, PecaAdv), TempTab1, TempTab2),
        peca_adversaria(Jog, PecaAdv),
        select((ColD, LinD, vazio), TempTab2, TempTab3),
        NovoTabTemp = [(ColD, LinD, Peca), (ColO, LinO, vazio),
                       (ColM, LinM, vazio)|TempTab3]
    ),
    promover_se_necessario(NovoTabTemp, Jog, (ColD, LinD), NovoTab).

% Verifica se a casa está vazia
casa_vazia(Tab, (Col, Lin)) :-
    member((Col, Lin, Peca), Tab), Peca == vazio.

% Promove a peça a dama se necessário
promover_se_necessario(Tab, Jog, (Col, Lin), NovoTab) :-
    (precisa_promover(Jog, Lin) ->
        select((Col, Lin, Peca), Tab, TempTab),
        (peça(Jog, Peca) ->
            dama(Jog, Dama),
            NovoTab = [(Col, Lin, Dama)|TempTab]
        ;   NovoTab = Tab)
    ;   NovoTab = Tab).

% Verifica se a peça precisa ser promovida
precisa_promover(jogador_a, 8).
precisa_promover(jogador_b, 1).

% Movimento válido para cada jogador
movimento_valido(Tab, Jog, (ColO, LinO), (ColD, LinD)) :-
    member((ColO, LinO, Peca), Tab),
    (dama(Jog, Peca) ->
        movimento_valido_dama(Tab, Jog, (ColO, LinO), (ColD, LinD))
    ;
        movimento_valido_peca(Tab, Jog, (ColO, LinO), (ColD, LinD))
    ).

% Movimento válido para a dama
movimento_valido_dama(Tab, _, (ColO, LinO), (ColD, LinD)) :-
    coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
    DifCol is NumColD - NumColO, DifLin is LinD - LinO,
    abs(DifCol, AbsDifCol), abs(DifLin, AbsDifLin),
    AbsDifCol =:= AbsDifLin, DirecaoCol is sign(DifCol),
    DirecaoLin is sign(DifLin),
    caminho_livre_dama(Tab, (NumColO, LinO), (NumColD, LinD),
                       DirecaoCol, DirecaoLin).

% Verifica se o caminho está livre para a dama
caminho_livre_dama(_, (NumColD, LinD), (NumColD, LinD), _, _) :- !.
caminho_livre_dama(Tab, (NumColA, LinA), (NumColD, LinD), DirCol, DirLin) :-
    NumColP is NumColA + DirCol, LinP is LinA + DirLin,
    coluna_num(ColP, NumColP), linha(LinP),
    (member((ColP, LinP, vazio), Tab) ->
        caminho_livre_dama(Tab, (NumColP, LinP), (NumColD, LinD),
                           DirCol, DirLin)
    ; fail
    ).

% Movimento válido para a peça normal
movimento_valido_peca(_, jogador_a, (ColO, LinO), (ColD, LinD)) :-
    coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
    LinD is LinO + 1,
    (NumColD is NumColO - 1 ; NumColD is NumColO + 1),
    linha(LinD), coluna(ColD), cor_casa(ColD, LinD, escura).

movimento_valido_peca(_, jogador_b, (ColO, LinO), (ColD, LinD)) :-
    coluna_num(ColO, NumColO), coluna_num(ColD, NumColD),
    LinD is LinO - 1,
    (NumColD is NumColO - 1 ; NumColD is NumColO + 1),
    linha(LinD), coluna(ColD), cor_casa(ColD, LinD, escura).

% Atualiza o tabuleiro após um movimento
atualiza_tabuleiro(Tab, (ColO, LinO), (ColD, LinD), Jog, NovoTab) :-
    select((ColO, LinO, Peca), Tab, TempTab1),
    select((ColD, LinD, vazio), TempTab1, TempTab2),
    NovoTabTemp = [(ColD, LinD, Peca), (ColO, LinO, vazio)|TempTab2],
    promover_se_necessario(NovoTabTemp, Jog, (ColD, LinD), NovoTab).

% Verifica se o movimento é válido
move_valido(Tab, Jog, (ColO, LinO), (ColD, LinD)) :-
    movimento_valido(Tab, Jog, (ColO, LinO), (ColD, LinD)),
    casa_vazia(Tab, (ColD, LinD)).

% Jogada do computador
jogador_programa(Tab, NovoTab, Jog) :-
    write('Vez do jogador A'), nl,
    findall((ColO, LinO, Captura),
        (member((ColO, LinO, Peca), Tab),
         peça(Jog, Peca),
         maior_captura(Tab, Jog, (ColO, LinO), Captura),
         Captura \= []
        ),
        CapturasPossiveis),
    (CapturasPossiveis \= [] ->
        selecionar_melhor_captura(CapturasPossiveis,
                                  (ColO, LinO, MelhorCaptura)),
        executar_captura(Tab, Jog, (ColO, LinO, MelhorCaptura),
                         NovoTab)
    ;   mover_peca(Tab, Jog, NovoTab)
    ).

% Seleciona a melhor captura
selecionar_melhor_captura(Capturas, (ColO, LinO, MelhorCapt)) :-
    Capturas = [(Col1, Lin1, Cap1)|Rest],
    selecionar_melhor_captura_aux(Rest, (Col1, Lin1, Cap1),
                                  (ColO, LinO, MelhorCapt)).

selecionar_melhor_captura_aux([], Melhor, Melhor).
selecionar_melhor_captura_aux([(Col, Lin, Cap)|Rest],
    (ColM, LinM, CapM), Resultado) :-
    length(Cap, Tamanho), length(CapM, TamM),
    (Tamanho > TamM ->
        selecionar_melhor_captura_aux(Rest, (Col, Lin, Cap), Resultado);
        selecionar_melhor_captura_aux(Rest, (ColM, LinM, CapM),
                                      Resultado)
    ).

% Executa a captura selecionada
executar_captura(Tab, Jog, (ColO, LinO, CapturaLista), NovoTab) :-
    atom_number(LinOAtom, LinO),
    atom_concat(ColO, LinOAtom, PosO),
    captura(Tab, Jog, (ColO, LinO), CapturaLista, NovoTab),
    format('Computador captura de ~w para ~w.~n', [PosO, CapturaLista]).

% Move uma peça para frente
mover_peca(Tab, Jog, NovoTab) :-
    findall((ColO, LinO, ColD, LinD),
            (move_possivel(Tab, Jog, (ColO, LinO), (ColD, LinD))),
            Movimentos),
    (Movimentos \= [] ->
        random_member((ColO, LinO, ColD, LinD), Movimentos),
        format('Computador move de (~w,~w) para (~w,~w).~n',
               [ColO, LinO, ColD, LinD]),
        atualiza_tabuleiro(Tab, (ColO, LinO), (ColD, LinD), Jog, NovoTab);
        NovoTab = Tab,
        write('Computador não tem movimentos válidos.'), nl
    ).

% Gera todos os movimentos possíveis
move_possivel(Tab, Jog, (ColO, LinO), (ColD, LinD)) :-
    member((ColO, LinO, Peca), Tab),
    peça(Jog, Peca),
    movimento_valido(Tab, Jog, (ColO, LinO), (ColD, LinD)),
    casa_vazia(Tab, (ColD, LinD)).

% Partida automática com pausa
partida_automatica(Tab, JogAtual) :-
    imprime_tabuleiro(Tab),
    aguardar_usuario,
    ( jogo_terminado(Tab, Venc) ->
        format('Jogo terminado! O vencedor é: ~w~n', [Venc]);
        jogador_programa(Tab, NovoTab, JogAtual),
        prox_jogador(JogAtual, ProxJog),
        partida_automatica(NovoTab, ProxJog)
    ).

% Aguarda o usuário pressionar Enter
aguardar_usuario :-
    write('Pressione Enter para o próximo movimento...'), nl,
    read_line_to_codes(user_input, _).

% Verifica se o jogo terminou
jogo_terminado(Tab, Vencedor) :-
    ( \+ member((_, _, peca_a), Tab), \+ member((_, _, dama_a), Tab) ->
        Vencedor = 'Jogador B';
      \+ member((_, _, peca_b), Tab), \+ member((_, _, dama_b), Tab) ->
        Vencedor = 'Jogador A';
      false
    ).
