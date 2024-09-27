/* ******************** Meta-Programação em Prolog *****************************

A ``atividade'' de criar programas cujo resultado são outros programas
é denominada de meta-programação.

Prolog possiu perdicados que dão suporte a meta-programação, permitindo
manipular a base de dados.

O predicado clause/2 relaciona a cabeça de uma regra com sua cauda.
Por exemplo, considere o programa a seguir
*/

father(X,Y) :- parent(X,Y), men(X).
father(adam,bill).

/* *************************** Exercício ****************************************

1- Realize a consulta clause(father(A,B), Z). Qual o resultado?

*********************************************************************************/

/* Resposta:

A consulta `clause(father(A,B), Z)` retorna:

- A = X, B = Y, Z = parent(X,Y), men(X).
- A = adam, B = bill, Z = true.

*/

/*
Os predicados asserta/1 e assertz/1 são utilizados para inserir
novas cláusulas na base de dados. Ambos diferem na posição que
a nova cláusula é inserida: no início da base para asserta e no
final para assertz

Considere a seguinte base de dados:
*/

% Estas cláusulas são necessárias para indicar ao Prolog que os predicados de meta-programação serão usados com void/0 e parent/2
:- dynamic void/0.
:- dynamic parent/2.

void :- assertz(void), fail.

parent(adam,bill).
parent(bill,cathy).

/* *************************** Exercício ****************************************

2- Qual o resultado das consultas?

a) parent(adam, X).
b) assertz(parent(adam,beth)), parent(adam,X).
c) parent(adam, X), asserta(parent(adam,beth)).

*********************************************************************************/

/* Resposta:

a) Retorna X = bill.

b) Retorna X = bill; X = beth.

c) Retorna X = bill.

*/

/* *************************** Exercício ****************************************

3- Qual o resultado da primeira consulta void. ? E uma segunda tentativa?

*********************************************************************************/

/* Resposta:

- Primeira tentativa: falha (false).
- Segunda tentativa: sucesso (true), pois o fato void foi adicionado.

*/

/*
Para remover cláusulas usamos o predicado retract/1
Este predicado irá remover a primeira cláusula unificada.
*/

:- dynamic friend/2.
:- dynamic likes/2.

friend(jin, james).
friend(jin, john).

likes(john, jin).
likes(james, john).

/* *************************** Exercício ****************************************

4- Realize a seguinte sequência de consultas:
   ?- friend(jin, X).
   ?- retract(friend(jin, james)).
   ?- friend(jin, X).
   ?- asserta((happy(X) :- friend(X,Y), likes(Y,X))).
   ?- happy(X).
   ?- retract(likes(X,Y)).
   ?- happy(jin).
   ?- likes(X,Y).

Qual o resultado desta sequência de operações?

*********************************************************************************/

/* Resposta:

- friend(jin, X). retorna X = james; X = john.
- Após retract(friend(jin, james)), friend(jin, X). retorna X = john.
- Após asserta(happy(X) :- friend(X,Y), likes(Y,X)), happy(X). retorna X = jin.
- Após retract(likes(X,Y)), happy(jin). falha.
- likes(X,Y). retorna X = james, Y = john.

*/

/* *************************** Exercício ****************************************

5- Escreva um predicado sigma/2 que tem como argumento um natural n > 0 e computa a soma
de todos os inteiros de 1 a n. Por exemplo:

  ?- sigma(3,X).

tem como resultado X = 6 e a consulta

  ?- sigma(5,X).

tem como resposta X = 15.

Escreva o predicado de tal forma que os valores computados são inseridos na base de dados e reusados quando possível.
Por exemplo, após as consultas sigma(3,X) e sigma(5,X), você deverá guardar na base os fatos:

   sigmares(3,6).
   sigmares(5,15).

*********************************************************************************/

:- dynamic sigmares/2.

sigma(N, Sum) :-
    sigmares(N, Sum), !.

sigma(0, 0) :-
    assertz(sigmares(0, 0)), !.

sigma(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sigma(N1, Sum1),
    Sum is Sum1 + N,
    assertz(sigmares(N, Sum)).

/* Exemplo de uso:

?- sigma(3, X).  % X = 6
?- sigma(5, X).  % X = 15

*/

/* Considere o programa */

child(martha,charlotte).
child(charlotte,caroline).
child(caroline,laura).
child(laura,rose).

descend(X,Y) :- child(X,Y).
descend(X,Y) :- child(X,Z), descend(Z,Y).

/* Considere o programa */

class(a, vow).
class(b, con).
class(c, con).
class(z, con).
class(d, con).
class(e, vow).
class(f, con).

/* *************************** Exercício ****************************************

6- Faça uma consulta para obter todas as vogais do programa acima.

*********************************************************************************/

/* Resposta:

Consulta:

?- findall(Letra, class(Letra, vow), Vogais).

Resultado:

Vogais = [a, e].

*/

/* *************************** Exercício ****************************************

7- Faça uma consulta para obter todas as letras, da base de dados class,
classificadas por vogais e consoantes. É possível obter a mesma resposta com os
itens ordenados?

*********************************************************************************/

/* Resposta:

Consulta:

?- bagof(Letra, class(Letra, Classe), Letras).

Resultados:

Classe = con, Letras = [b, c, z, d, f];
Classe = vow, Letras = [a, e].

Com itens ordenados:

?- setof(Letra, class(Letra, Classe), Letras).

Resultados:

Classe = con, Letras = [b, c, d, f, z];
Classe = vow, Letras = [a, e].

Sim, usando setof/3.

*/

/* ******************** Sistemas Especialistas **********************************

                   VER APRESENTAÇÃO DO MATERIAL DA AULA
*/

:- op(800, yfx, if).
:- op(200, xfy, and).

:- dynamic clause/1.

clause(animal(dog) if tem(pele) and late).
clause(animal(cat) if tem(pele) and mia).
clause(animal(duck) if tem(asas) and faz(quack)).

solve(true).
solve(X and Y) :- solve(X), solve(Y).
solve(X) :- clause(X if Y), solve(Y).
solve(X) :- clause(X if true).

question(tem(pele)) :-
    write('O animal tem pele? (yes/no)\n'),
    read(Resposta),
    (Resposta = yes -> asserta(clause(tem(pele) if true)); fail).
question(late) :-
    write('O animal late? (yes/no)\n'),
    read(Resposta),
    (Resposta = yes -> asserta(clause(late if true)); fail).
question(mia) :-
    write('O animal mia? (yes/no)\n'),
    read(Resposta),
    (Resposta = yes -> asserta(clause(mia if true)); fail).
question(tem(asas)) :-
    write('O animal tem asas? (yes/no)\n'),
    read(Resposta),
    (Resposta = yes -> asserta(clause(tem(asas) if true)); fail).
question(faz(quack)) :-
    write('O animal faz quack? (yes/no)\n'),
    read(Resposta),
    (Resposta = yes -> asserta(clause(faz(quack) if true)); fail).

expert(X) :-
    solve(animal(X)),
    write('O animal é um '), write(X), nl.
