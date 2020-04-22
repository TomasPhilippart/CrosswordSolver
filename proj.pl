/* 95683, Tomas Philippart */

:- [codigo_comum].

/* =========== obtem_letras_palavras(Lst_Pals, Letras) ==============
    em que Lst_Pals e uma lista de palavras, significa que Letras e 
    a lista ordenada cujos elementos sao listas com as letras de 
    cada palavra de Lst_Pals
---------------------------------------------------------------------- */

obtem_letras_palavras(Lst_Pals, Letras_Ord) :-
    /* transforma todos os elementos de Lst_Pals numa lista dos seus carateres */
    maplist(atom_chars, Lst_Pals, Letras),
    
    /* ordenada a lista por ordem alfabetica */
    sort(Letras, Letras_Ord).



/* ================= espaco_fila(Fila, Espacos) =====================
    em que Fila e uma fila (linha ou coluna) de uma grelha,
    significa que Esp e um espaco de Fila
---------------------------------------------------------------------- */

espaco_fila(Fila, Esp) :- espaco_fila_aux(Fila, [], Esp).

/* se a fila for vazia */
espaco_fila_aux([], Aux, Aux) :-
    /* verificar se ha pelo menos 3 elementos */
    length(Aux, Cont),
    Cont >= 3, !.

/* quando o elemento a ver for diferente de #.
  -> NOTA: devemos testar primeiro este caso, de forma a 
           prevenir a unificacao da variavel anonima com o #. */
espaco_fila_aux([H | T], Aux, Esp) :-
    H \== #,
    append(Aux, [H], Aux_Atualizado), 
    espaco_fila_aux(T, Aux_Atualizado, Esp).

/* quando o elemento a ver for o # */
espaco_fila_aux([H | _], Aux, Aux) :-
    H == #,
	/* verificar se ha pelo menos 3 elementos */
    length(Aux, Cont),
    Cont >= 3.

/* quando o elemento a ver for o # e houver menos de 3 elementos*/
espaco_fila_aux([H | T], _, Esp) :-
    H == #,
    /* reinicia Aux a [] */
    espaco_fila_aux(T, [], Esp).


/* ================= espacos_fila(Fila, Espacos) =====================
    em que Fila e uma fila (linha ou coluna) de uma
    grelha, significa que Espacos e a lista de todos os espacos de
    Fila, da esquerda para a direita
---------------------------------------------------------------------- */

espacos_fila(Fila, L_Espacos) :-
   	/* produz uma lista para os quais as sublistas sao respostas valida para espacos */ 
    bagof(Esp, espaco_fila(Fila, Esp), L_Espacos), !.

/* no caso em que a fila nao tem qualquer espaco,
   devolver lista vazia */
espacos_fila(_, []).

/* ================= espacos_puzzle(Grelha, Espacos) =================
    funcao que verifica se Espacos e a lista de espacos da Grelha
---------------------------------------------------------------------- */

/* funcao aux: alisa um nivel
        nota: esta funcao aux so server para melhorar a legibilidade do codigo */
alisa_1_nivel(L, L_alisada) :- append(L, L_alisada).

espacos_puzzle(Grelha, Espacos) :- 
    
    /* linhas da grelha */
    maplist(espacos_fila, Grelha, Esp_Linhas_Aux),
    alisa_1_nivel(Esp_Linhas_Aux, Espacos_Linhas),

	/* colunas da grelha (linhas da matriz composta da grelha) */
	mat_transposta(Grelha, Grelha_T),
	maplist(espacos_fila, Grelha_T, Esp_Colunas_Aux),
    alisa_1_nivel(Esp_Colunas_Aux, Espacos_Colunas),
    
    /* junta os espacos das linhas e das colunas */
	append(Espacos_Linhas, Espacos_Colunas, Esp_Aux),

    /* exclui listas vazias */
    exclude(==([]), Esp_Aux, Espacos).

/* ======= espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) =======
        funcao que verifica se os espacos Esps_com sao espacos com
        posicoes comuns a Esp, contidos na lista de espacos Espacos
---------------------------------------------------------------------- */

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :- 

    /* inclui todos os elementos de Espacos em Espacos_aux, se 
        existir intersecao (posicoes em comum) entre eles */
    include(existe_intersecao(Esp), Espacos, Espacos_aux),

    /* excluir Esp de Espacos_aux */
    exclude(==(Esp), Espacos_aux, Esps_com).


/* funcao aux: verifica se ha intersecao entre uma 2 listas*/
existe_intersecao([X | _], L) :- pertence(X, L), !.
existe_intersecao([_ | R], L) :- existe_intersecao(R, L).


/* funcao aux: verifica se um elemento pertence a uma lista.
    alternativa a funcao member, que unifica (nao desejavel neste caso) */
pertence(E, [H | _]) :- E == H, !.
pertence(E, [_ | T]) :- pertence(E, T).


/* ======== palavra_possivel_esp(Pal, Esp, Espacos, Letras) ==========
                funcao que verifica se a palavra Pal e 
                uma palavra possivel para o espaco Esp
---------------------------------------------------------------------- */

/* funcao aux: verifica se X e Y sao unificaveis, sem unificar */
unificavel(X, Y) :- \+ (X \= Y).

palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-

    /* verificar que Esp pertence a Espacos */
    pertence(Esp, Espacos),

    /* verificar que Pal pertence a Letras */
    member(Pal, Letras), 
    
    /* verificar que Pal e Esp sao unificaveis */
    Pal = Esp,
    
    /* Letras_atualizado corresponde a remocao de Pal de Letras */
    /* MUITO ESQUISITO, PUS EM COMENTARIO POR CAUSA DO DAO */
    /* selectchk(Pal, Letras, Letras_atualizado), */

    /* Esps_com sao os espacos comuns de Esp */
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    
    /* para cada espaco em comum, ver se ha uma palavra possivel,
       sem entrar em conflito com as letras de Pal nas posicoes comuns */
    validar_esps_comuns(Esps_com, Letras).


/* funcao aux: para cada espaco em comum, ve se ha uma palavra possivel */
/* caso terminal: se nao houverem espacos comuns */
validar_esps_comuns([], _).
validar_esps_comuns([H_Esps | T_Esps], Letras) :-

    /* verificar que Palavra e membro de Letras e diferente de Pal */
    member(Palavra, Letras),

    /* verificar que Palavra e unificavel com H_Esps */
    unificavel(Palavra, H_Esps), !,

    /* retira Palavra da lista de Letras a ver */
    selectchk(Palavra, Letras, Letras_atualizado),

    /* verifica se ha alguma Palavra para o resto da lista de espacos */
    validar_esps_comuns(T_Esps, Letras_atualizado).

/* ======== palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) ==========
                funcao que verifica se Pals possiveis e uma lista
                ordenada de palavras possiveis para o espaco Esp
----------------------------------------------------------------------------------- */

palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal, palavra_possivel_esp(Pal, Esp, Espacos, Letras), Pals_Possiveis).

/* ======== palavras_possiveis(Letras, Espacos, Pals_Possiveis) ===================
            funcao que verifica se Pals possiveis e uma lista
            ordenada de palavras possiveis para cada espaco de Espacos
----------------------------------------------------------------------------------- */

/* funcao aux: simplesmente usada para concatenar o espaco e 
as posicoes possiveis numa unica lista*/
palavras_possiveis_aux(Letras, Espacos, Esp, L_Esp_Pals) :-
    /* L_Esp_Pals corresponde a [Esp | Pals_Possiveis(esp)] */
    append(Esp, Pals_Possiveis, L_Esp_Pals),
    palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis).


palavras_possiveis(Letras, Espacos, L_Esp_Pals) :- 
    /* n funciona, falta incluir o espaco com a palavra*/
    maplist(palavras_possiveis_aux(Letras, Espacos), Espacos, L_Esp_Pals).    



/* =============== letras_comuns(Lst_Pals, Letras_comuns) =========================
            Letras_comuns corresponde a lista de pares (pos, letra),
            significando que todas as listas de Lst_Pals, contem a 
            letra na posicao pos.
----------------------------------------------------------------------------------- */

/* exemplo: Lst_Pals = [[a,t,o], [a, c, o], [a,n,o], [a,l,o]]
            Letras_comuns = [(1, a),  (3, o)]*/


letras_comuns(Lst_Pals, Letras_comuns) :-

    /* fazer transposta da Lst_Pals, ficando cada sublista com a Letra de 
    cada palavra na mesma Posicao. ex: [[a,t,o], [a, c, o], [a,n,o], [a,l,o]] 
                               -> [[a, a, a, a], [t, c, n, l], [o, o, o, o]] */
    mat_transposta(Lst_Pals, Lst_Pals_T),

    /* lista de 1 a Len*/
    length(Lst_Pals_T, Len),
    numlist(1, Len, Posicoes),

    maplist(elementos_iguais, Posicoes, Lst_Pals_T, Letras_comuns_aux),

    /* exclui listas vazias */
    exclude(==([]), Letras_comuns_aux, Letras_comuns).

/* funcao aux: verifica que todos os elementos duma lista sao iguais */
elementos_iguais(Pos, Letras, Par) :-

    /* verifica que todos as letras na lista sao iguais */
    maplist(=(_), Letras), !,

    /* Letra corresponde a letra da lista*/
    nth1(1, Letras, Letra),

    Par = (Pos, Letra).

/* no caso em que nem todas as letras forem iguais, retorna []*/
elementos_iguais(_, _, []).


/* =============== atribui_comuns(Pals_Possiveis) ================================
            Atualiza Pals_Possiveis, atribuindo a cada espaco as letras
            comuns a todas as palavras possiveis para o tal espaco
----------------------------------------------------------------------------------- */

/* funcao aux: substitui(Lista, Indice, Valor, NovaLista), em que NovaLista corresponde
               a Lista, com o elemento em Indice igual a Valor. */

substitui([_ | T], 1, Valor, [Valor | T]). /* Indice = 1 */
substitui([H | T], Indice, Valor, [H | R]) :- 
    Indice >= 1, 
    Novo_Indice is Indice-1, 
    substitui(T, Novo_Indice, Valor, R), !.
substitui(Lista, _, _, Lista).


/* funcao aux: atribui a um Espaco pares (Pos, Letra),
               substituindo as posicoes Pos por Letra. */
atribui(Esp, [H_Letras_com | T_Letras_com], Esp_atual) :-
    /* retirar Pos e Letra do par, recorrendo a unificacao */
    (Pos, Letra) = H_Letras_com,
    substitui(Esp, Pos, Letra, Esp_atual),
    /* TODO*/


