-module(sdk).

-export([solve/0, test/0, revision_step/3, get_possibilities/2, keep_diff_than/2, keep_greater_than/2, possibilities/1, board/0, update_possibilities/3, positions/0, arc_consistency/1, all_pairs/0, pair_w_neighbors/2, get_contraint/3, constraints/0, invalid/1, done/1, extract_solution/1, find_choice/2, grid/1]).

-import(lists, [nth/2, usort/1, map/2, seq/2, nth/1, merge/2]).

board() -> % tabuleiro a ser resulvido

[
    [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [8, 0, 0, 0, 3, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 3, 0, 0, 6, 0, 0, 0]
  , [1, 0, 0, 0, 0, 0, 0, 0, 0]
].

constraints() -> % restrições do tabuleiro

    [{0,1,1}, {1,2,0}, {3,4,0}, {4,5,1}, {6,7,0}, {7,8,1}
    , {0,9,1}, {1,10,0}, {2,11,0}, {3,12,1}, {4,13,1}, {5,14,0}, {6,15,0}, {7,16,1}, {8,17,0}
    , {9,10,1}, {10,11,0}, {12,13,0}, {13,14,0}, {15,16,1}, {16,17,0}
    , {9,18,0}, {10,19,0}, {11,20,1}, {12,21,0}, {13,22,0}, {14, 23,1}, {15,24,1}, {16,25,0}, {17,26,0}
    , {18,19,1}, {19,20,1}, {21,22,0}, {22,23,1}, {24,25,0}, {25,26,0}
    , {27,28, 0}, {28,29,1}, {30,31,0}, {31,32,1}, {33,34,1}, {34,35,0}
    , {27,36,0}, {28,37,1}, {29,38,1}, {30,39,0}, {31,40,1}, {32,41,0}, {33,42,1}, {34,43,0}, {35,44,1}
    , {36,37,1}, {37,38,1}, {39,40,1}, {40,41,0}, {42,43,0}, {43,44,1}
    , {36,45,1}, {37,46,1}, {38,47,0}, {39,48,0}, {40,49,0}, {41,50,0}, {42,51,0}, {43,52,1}, {44,53,0}
    , {45,46,1}, {46,47,0}, {48,49,0}, {49,50,1}, {51,52,1}, {52,53,0}
    , {54,55,0}, {55,56,1}, {57,58,1}, {58,59,0}, {60,61,1}, {61,62,0}
    , {54,63,1}, {55,64,0}, {56,65,0}, {57,66,1}, {58,67,0}, {59,68,0}, {60,69,0}, {61,70,1}, {62,71,1}
    , {63,64,0}, {64,65,1}, {66,67,1}, {67,68,0}, {69,70,1}, {70,71,0}
    , {63,72,1}, {64,73,1}, {65,74,0}, {66,75,0}, {67,76,1}, {68,77,1}, {69,78,1}, {70,79,0}, {71,80,0}
    , {72,73,0}, {73,74,0}, {75,76,1}, {76,77,0}, {78,79,0}, {79,80,1}
    ].

test() ->

    possibilities(board()).

positions() -> % gera uma lista com todas as posições do tabuleiro

    [{X, Y} || X <- seq(0,8), Y <- seq(0,8)].

possibilities(Matrix) -> % gera a tabela de possibilidades de um tabuleiro

    map(fun(Row) -> map(fun(N) -> possibilities_h(N) end, Row) end, Matrix).

possibilities_h(N) -> % função auxiliar de possibilities | determina as possibilidades para uma célula do tabuleiro inicial

    if
        N =:= 0 -> seq(1, 9);
        true -> [N]
    end.

get_possibilities({I, J}, T) -> % recupera a lista de possibilidades de uma posição específica do tabuleiro

    Row = nth(I + 1, T),
    nth(J + 1, Row).

update_possibilities({I, J}, NewList, PossibilitiesMatrix) -> % atualisa as possibilidades de uma célular

    lists:sublist(PossibilitiesMatrix, I - 1) ++
    [update_row(lists:nth(I, PossibilitiesMatrix), J, NewList)] ++
    lists:nthtail(I, PossibilitiesMatrix).

update_row(Row, RowIndex, NewValues) -> % helper de update_possibilities

    lists:sublist(Row, RowIndex - 1) ++
    [NewValues] ++
    lists:nthtail(RowIndex, Row).

keep_lower_than([Xh | Xs], N) -> % retira possibilidades caso o elemnto seja maior que n

    if 
        Xh < N -> [Xh] ++ keep_lower_than(Xs, N);
        true -> keep_lower_than(Xs, N)
    end;
keep_lower_than([], _) -> [].

keep_greater_than([Xh | Xs], N) -> % retira possibilidades caso o elemnto seja menor que n

    if 
        Xh > N -> [Xh] ++ keep_greater_than(Xs, N);
        true -> keep_greater_than(Xs, N)
    end;
keep_greater_than([], _) -> [].

keep_diff_than([], _) -> [];
keep_diff_than([Xh | Xs], N) -> % retira possibidades caso o elemento seja igualm a n

    if 
        Xh /= N -> [Xh] ++ keep_diff_than(Xs, N);
        true -> keep_diff_than(Xs, N)
    end.
        
revision_step(_, [], _) -> [];
revision_step(X, [Y|Ys], N) -> % revisa a consistencia entre x e y

    case N of
        % case -1
        -1 -> X;
        % case 0
        0 -> usort(keep_lower_than(X, Y) ++ revision_step(X, Ys, N));
        % case 1
        1 -> usort(keep_greater_than(X, Y) ++ revision_step(X, Ys, N));
        % case > 2
        2 -> usort(keep_diff_than(X, Y) ++ revision_step(X, Ys, N))
    end.

get_linear_coord({I, J}) -> % retorna a posição em formato linearizado

    I * 9 + J.

get_contraint([], _, _) -> 2;
get_contraint([{X, Y, V}|C], P1, P2) ->

    Px = get_linear_coord(P1),
    Py = get_linear_coord(P2),

    if 
        % caso encontre a restrição
        X == Px, Y == Py -> V;
        % caso encontre a restrição reversa
        X == Py, Y == Px, V == 1 -> 0;
        X == Py, Y == Px, V == 0 -> 1;
        % caso não tenha econtrado a restrição continua procurando
        true -> get_contraint(C, P1, P2)
    end.

grid({I, J}) when I >= 0, I =< 8, J >= 0, J =< 8 -> % helper de neighbors | gera uma lista de todas as posições que fazem parte do mesmo grid que a posição passada por parâmetro
    SubgridRow = (I div 3) * 3 + 1,
    SubgridCol = (J div 3) * 3 + 1,
    Positions = [{X, Y} || X <- seq(SubgridRow - 1, SubgridRow + 1), Y <- seq(SubgridCol - 1, SubgridCol + 1)],
    Positions -- [{I, J}].

neighbors({I, J}) -> % lista todas as posições que se relacionam com  posição (I, J)
    Grid = grid({I, J}),
    Row = [{I, X} || X <- seq(0,8)] -- [{I, J}],
    Col = [{X, J} || X <- seq(0,8)] -- [{I, J}],
    N = Grid ++ Row ++ Col,

    lists:usort(N).

pair_up(_, []) -> [];
pair_up(P, [H | T]) -> % retorna uma lista de pares P com cada elemento da lista recebida por parâmetro

    [{P, H}] ++ pair_up(P, T).

rev_pair_up(_, []) -> [];
rev_pair_up(P, [H | T]) -> % par invertido 

    [{H, P}] ++ rev_pair_up(P, T).

pair_w_neighbors(P, R) -> % retorna uma lista de pares de P com todos seus vizinhos do tabuleiro

    N = neighbors(P),

    case R of
        % caso padrão
        0 -> pair_up(P, N);
        % ordem invertida
        1 -> rev_pair_up(P, N)
    end.

all_pairs() -> % retorna uma lista com todos os pares com relações no tabuleiro

    P = positions(),
    all_pairs_h(P).

all_pairs_h([]) -> [];
all_pairs_h([Po | P]) -> % helper de all_pairs

    Pairs = pair_w_neighbors(Po, 0),

    merge(Pairs, all_pairs_h(P)).

arc_consistency(Ptable) ->

    Pairs = all_pairs(),

    arc_consistency_h(Pairs, Ptable).

arc_consistency_h([], Ptable) -> Ptable;
arc_consistency_h([{P1, P2} | Xs], Ptable) ->

    % recupera as possibilidades das posições
    Possib1 = get_possibilities(P1, Ptable),
    Possib2 = get_possibilities(P2, Ptable),
    % recupera a restrição entre as posições
    Constraint = get_contraint(constraints(), P1, P2),
    % revisa as possibilidades de P1
    Revised = revision_step(Possib1, Possib2, Constraint),
    % diferença entre as possibilidades antes e depois da revisão
    Diff = Revised -- Possib1,

    if
        % se não houver valor válido para a posição, então retorna a nova tabela de possibilidades que representa a impossibilidade
        Revised == [] -> 
            {I, J} = P1,
            I1 = I + 1,
            J1 = J + 1,
            update_possibilities({I1, J1}, Revised, Ptable);
        % se não houver alteração nas possibilidades de P1, então continua as revisões
        Diff == [] ->
            {I, J} = P1,
            I1 = I + 1,
            J1 = J + 1,  
            Newtable = update_possibilities({I1, J1}, Revised, Ptable),
            arc_consistency_h(Xs, Newtable);
        % se ocorrerem alterações nas possibilidades de P1, adiciona os vizinhos de P1 a fila e continua as revisões
        true -> 
            {I, J} = P1,
            I1 = I + 1,
            J1 = J + 1,  
            Newtable = update_possibilities({I1, J1}, Revised, Ptable),
            arc_consistency_h(Xs ++ pair_w_neighbors(P1, 1), Newtable)
    end.

invalid(P) -> % confere a validade da tabela de possibilidades
    lists:any(fun(Row) -> lists:any(fun(Cell) -> Cell == [] end, Row) end, P).

done(P) -> % verifica se uma solução está pronta a partir da sua tabela de possibilidades
    lists:all(fun(Cell) -> is_list(Cell) andalso length_one_recursive(Cell) end, P).

length_one_recursive([]) -> true; % helper de done
length_one_recursive([H | T]) when is_list(H) -> length(H) == 1 andalso length_one_recursive(T);
length_one_recursive(_) -> false.

extract_solution(Matrix) -> % transforma uma tabela de possibilidades de um tabuleiro pronto em uma matriz que representa o tabuleiro
    lists:map(fun(Row) -> flatten_row(Row) end, Matrix).

flatten_row(Row) -> % helper de extract_solution
    lists:foldl(fun(Cell, Acc) -> Acc ++ Cell end, [], Row).

find_choice(PositionList, Matrix) -> % acha a primeira posição não decidida da matrix
    {I, J} = find_choice(PositionList, Matrix, []),
    {I - 1, J - 1}.

find_choice([], _, Result) when Result /= [] -> % faz a recursão de find_choice
    hd(Result);
find_choice([], _, _) ->
    {0, 0};
find_choice([{I, J} | Rest], Matrix, Result) ->
    case is_valid_position({I, J}, Matrix) of
        true ->
            case length_of_element({I, J}, Matrix) > 1 of
                true -> {I, J};
                false -> find_choice(Rest, Matrix, Result)
            end;
        false -> find_choice(Rest, Matrix, Result)
    end.

is_valid_position({I, J}, Matrix) when I >= 1, J >= 1, I =< length(Matrix), J =< length(hd(Matrix)) ->  % helper de find_choice
    true;
is_valid_position(_, _) ->
    false.

length_of_element({I, J}, Matrix) -> % helper de find_choice
    length(lists:nth(J, lists:nth(I, Matrix))).

try_choices(_, [], _) -> nothing; % helper de backtracking | realiza as escolhas
try_choices(Position, [Possibility | Tail], P) ->
    {I, J} = Position,
    Table = update_possibilities({I + 1, J + 1}, [Possibility], P),
    Newtable = arc_consistency(Table),
    Solution = backtracking(Newtable), 

    if
        Solution == nothing -> try_choices(Position, Tail, P);
        true -> Solution
    end.

backtracking(P) -> % usa uma técnica de backtracking para resolver o puzzle
    Case1 = invalid(P),
    Case2 = done(P),

    if
        Case1 -> nothing;
        Case2 -> extract_solution(P);
        true -> 
            Position = find_choice(positions(), P),
            Possibilities = get_possibilities(Position, P),
            try_choices(Position, Possibilities, P)
    end. 

solve() -> % resolve o puzzle (chama o backtracking)
    B = board(),
    Ptable = possibilities(B),
    backtracking(Ptable).

% TODO - resolver problema das tentativas | backtracking para de volta direito