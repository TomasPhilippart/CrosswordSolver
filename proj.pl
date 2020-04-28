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
palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-

    /* verificar que Esp pertence a Espacos */
    pertence(Esp, Espacos),

    /* verificar que Pal pertence a Letras */
    member(Pal, Letras), 
    
    /* verificar que Pal e Esp sao unificaveis */
    Pal = Esp,

    /* Esps_com sao os espacos comuns de Esp */
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    
    /* para cada espaco em comum, ver se ha uma palavra possivel,
       sem entrar em conflito com as letras de Pal nas posicoes comuns */
    validar_esps_comuns(Esps_com, Letras).

/* funcao aux: verifica se X e Y sao unificaveis, sem unificar */
unificavel(X, Y) :- \+ (X \= Y).

/* funcao aux: para cada espaco em comum, ve se ha uma palavra possivel */
/* caso terminal: se nao houverem espacos comuns */
validar_esps_comuns([], _).
validar_esps_comuns([H_Esps | T_Esps], Letras) :-

    /* verificar que Palavra e membro de Letras e diferente de Pal */
    member(Palavra, Letras),

    /* verificar que Palavra e unificavel com H_Esps */
    unificavel(Palavra, H_Esps), !,
    
    /* verifica se ha alguma Palavra para o resto da lista de espacos */
    validar_esps_comuns(T_Esps, Letras).

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

palavras_possiveis(Letras, Espacos, L_Esp_Pals) :- 
    maplist(palavras_possiveis_aux(Letras, Espacos), Espacos, L_Esp_Pals).  

/* funcao aux: simplesmente usada para concatenar o espaco e 
as palavras possiveis numa unica lista*/
palavras_possiveis_aux(Letras, Espacos, Esp, L_Esp_Pals) :-

    /* L_Esp_Pals corresponde a [Esp | Pals_Possiveis(esp)] */
    append([Esp], [Pals_Possiveis], L_Esp_Pals),
    palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis).


/* =============== letras_comuns(Lst_Pals, Letras_comuns) =========================
            Letras_comuns corresponde a lista de pares (pos, letra),
            significando que todas as listas de Lst_Pals, contem a 
            letra na posicao pos.
----------------------------------------------------------------------------------- */

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

atribui_comuns([]).
atribui_comuns([H_Esp_Pals | T_Esps_Pals]) :-

    /* separa Espaco e Palavras do elemento */
    [Esp, Palavras] = H_Esp_Pals,

    letras_comuns(Palavras, Letras_com),

    atribui(Esp, Letras_com),

    /* processa o resto da lista de Pals_Possiveis */
    atribui_comuns(T_Esps_Pals), !.


/* funcao aux: atribui a um Espaco pares (Pos, Letra),
               substituindo as posicoes Pos por Letra. */
atribui(_, []).
atribui(Esp, [H_Letras_com | T_Letras_com]) :-

    /* retirar Pos e Letra do par, recorrendo a unificacao */
    (Pos, Letra) = H_Letras_com,

    /* substitui o Espaco com a letra*/
    nth1(Pos, Esp, Letra),

    atribui(Esp, T_Letras_com), !.


/* ========= retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) ==================
            em que Novas_Pals_Possiveis e o resultado de tirar palavras
            impossiveis da Pals_Possiveis
--------------------------------------------------------------------------------------- */

retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :- 
    retira_impossiveis_aux(Pals_Possiveis, [], Novas_Pals_Possiveis).

/* funcao aux iterativa */
retira_impossiveis_aux([], Aux, Aux).
retira_impossiveis_aux([H_Pals_Possiveis | T_Pals_Possiveis], Aux, Novas_Pals_Possiveis) :-
    
    [Esp, Palavras] = H_Pals_Possiveis,

    /* Pals_Possiveis corresponde as palavras 
    possiveis da lista Palavras para o Espaco Esp */
    possiveis(Esp, Palavras, [], Pals_Possiveis),

    /* Acrescentar a lista Auxiliar a Pals_Possiveis, atualizando*/
	append(Aux, [Pals_Possiveis], Pals_Possiveis_atualizada),
    
    retira_impossiveis_aux(T_Pals_Possiveis, Pals_Possiveis_atualizada, Novas_Pals_Possiveis).

/* funcao aux: Pals_Possiveis corresponde as palavras possiveis para Esp*/
possiveis(Esp, [], Aux, [Esp, Aux]). /* caso terminal */
possiveis(Esp, [H_Palavras | T_Palavras], Aux, Pals_Possiveis) :-

    /* verificar que Esp e H_Palavras sao unificaveis */
    unificavel(Esp, H_Palavras),

    /* adicionar as Possiveis a Aux */
    append(Aux, [H_Palavras], Aux_Atualizado),

    possiveis(Esp, T_Palavras, Aux_Atualizado, Pals_Possiveis), !.
    
/* caso em que a letra e impossivel*/
possiveis(Esp, [_ | T_Palavras], Aux, Pals_Possiveis) :-
    /* continuar para o resto das palavras*/
    possiveis(Esp, T_Palavras, Aux, Pals_Possiveis).

/* =============== obtem_unicas(Pals_Possiveis, Unicas) ===============================
            em que Unicas e a lista de palavras unicas 
--------------------------------------------------------------------------------------- */

obtem_unicas(Pals_Possiveis, Unicas) :-
    include(eh_unica, Pals_Possiveis, Unicas_esp_pal),
    /* retira Esp de Unicas_aux, ficando so com a lista de palavras */
    maplist(nth1(2), Unicas_esp_pal, Unicas_aux),
    alisa_1_nivel(Unicas_aux, Unicas).

/* funcao aux: devolve True se L_Esp_Pals tiver uma unica palavra possivel */
eh_unica(L_Esp_Pals) :-
    nth1(2, L_Esp_Pals, Pal),
    length(Pal, 1).

/* =============== retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) ================
            em que Novas_Pals_Possiveis e o resultado de 
            retirar palavras unicas de Pals_Possiveis
--------------------------------------------------------------------------------------- */

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, Unicas),
    maplist(retira_aux(Unicas), Pals_Possiveis, Novas_Pals_Possiveis).

/* funcao aux: retira de uma lista [Esp, Palavras] as palavras
               da lista Unicas, caso essa lista tenha varias palavras*/
retira_aux(Unicas, L_Esp_Pals, Novo_L_Esp_Pals) :-

    [Esp, Palavras] = L_Esp_Pals,

    /* verificar que ha mais de uma palavra*/
    length(Palavras, N_Palavras),
    N_Palavras > 1, !,

    /* Aux corresponde a lista de Palavras,
       sem as Unicas, se as contiver */
    subtract(Palavras, Unicas, Aux),

    append([Esp], [Aux], Novo_L_Esp_Pals), !.

retira_aux(_, L, L).

/* =============== simplifica(Pals_Possiveis, Novas_Pals_Possiveis) ===================
            em que Novas_Pals_Possiveis e o resultado de 
            simplificar Pals_Possiveis
--------------------------------------------------------------------------------------- */

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-

    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Aux),
    retira_unicas(Aux, Aux_Atualizado),

    /* verificar que houve alteracao */
    Aux_Atualizado \== Pals_Possiveis, !,

    simplifica(Aux_Atualizado, Novas_Pals_Possiveis).

/* se nao houver alteracao, atribuir comuns */
simplifica(Pals_Possiveis, Pals_Possiveis) :- atribui_comuns(Pals_Possiveis).


/* ================== inicializa(Puz, Pals_Possiveis) ================================
            em que Puz e um puzzle, significa que Pals_Possiveis
            e a lista de palavras possiveis simplificadas para Puz 
--------------------------------------------------------------------------------------- */

inicializa(Puz, Pals_Possiveis) :-

    [Lst_Pals, Grelha] = Puz,

    obtem_letras_palavras(Lst_Pals, Palavras),
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Palavras, Espacos, Aux),
    simplifica(Aux, Pals_Possiveis).


/* ============= escolhe_menos_alternativas(Pals_Possiveis, Escolha) ==================
            em que Escolha e o elemento de Pals_Possiveis escolhido
            segundo o criterio da seccao 2.2.1.
--------------------------------------------------------------------------------------- */

escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-

    /* por default, Escolha_aux e um elemento de Pals_Possiveis*/
    member(Escolha_aux, Pals_Possiveis),

    /* nao pode ter uma lista de palavras unitaria*/
    nth1(2, Escolha_aux, Palavras_aux),
    length(Palavras_aux, Num_Pals_aux),
    Num_Pals_aux =\= 1,

    escolhe_aux(Pals_Possiveis, Escolha_aux, Escolha), !.

escolhe_aux([H_Pals_Possiveis | T_Pals_Possiveis], Escolha_aux, Escolha) :-

    nth1(2, H_Pals_Possiveis, Palavras),
    length(Palavras, Num_Pals),

    nth1(2, Escolha_aux, Palavras_aux),
    length(Palavras_aux, Num_Pals_aux),

    /* verificar que a lista de palavras nao e unitaria*/
    Num_Pals =\= 1,

    /* se o numero de palavras for menor, passa a ser a nova Escolha aux */
    Num_Pals < Num_Pals_aux, !,

    escolhe_aux(T_Pals_Possiveis, H_Pals_Possiveis, Escolha).


escolhe_aux([_ | T_Pals_Possiveis], Escolha_aux, Escolha) :-
    escolhe_aux(T_Pals_Possiveis, Escolha_aux, Escolha).

/* caso terminal */
escolhe_aux([], Escolha_aux, Escolha_aux).

/* =========== experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) ==========
            em que Escolha e um dos elementos de Pals_Possiveis,
            Novas_Pals_Possiveis e o resultado de substituir, em 
            Pals_Possiveis, o elemento escolha pelo elemento [Esp, [Pal]].
--------------------------------------------------------------------------------------- */

experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) :-
    maplist(experimenta_substituir(Escolha), Pals_Possiveis, Novas_Pals_Possiveis).

/* funcao aux: substitui Nova_Esp_Pal por Escolha ou copia */
experimenta_substituir(Escolha, Esp_Pal, Nova_Esp_Pal) :-

    Escolha == Esp_Pal, !,
    Escolha = [_, Palavras],
    member(Palavra, Palavras),

    /* unificar Esp com Pal*/
    Esp_Pal = [Palavra, Palavras],
    
    Nova_Esp_Pal = [Palavra, [Palavra]].
    
/* caso em que Escolha e diferente de Esp_Pal, copiar Esp_Pal*/
experimenta_substituir(_, Esp_Pal, Esp_Pal).


/* =========== resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) ======================
            em que Novas_Pals_Possiveis e o resultado de aplicar o 
            algoritmo da seccao 2.2 a Pals_Possiveis
--------------------------------------------------------------------------------------- */

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-

    escolhe_menos_alternativas(Pals_Possiveis, Escolha), !,
    experimenta_pal(Escolha, Pals_Possiveis, Aux),
    simplifica(Aux, Aux_Atualizado),

    resolve_aux(Aux_Atualizado, Novas_Pals_Possiveis).

/* se todos os espacos ja tiverem uma palavra */
resolve_aux(Pals_Possiveis, Pals_Possiveis).

/* ========================== resolve(Puz) ============================================
            em que Puz e um puzzle, resolve esse Puzzle, substituindo 
            todas as variaveis por letras que  constituitem as palavras 
            da lista de palavras de Puz.
--------------------------------------------------------------------------------------- */

resolve(Puz) :-
    inicializa(Puz, Pals_Possiveis),
    resolve_aux(Pals_Possiveis, _).
   
