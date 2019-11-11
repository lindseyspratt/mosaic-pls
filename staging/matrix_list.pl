:- module(matrix_list, []).

% adjacency_matrix([1,2,3], [edge(1,2), edge(2,3)], Matrix)
% adjacency_matrix([1,2,3,4], [edge(1,2), edge(2,3), edge(3,4)], Matrix)
% adjacency_matrix([1,2,3,4], [edge(1,2), edge(2,4)], Matrix)
adjacency_matrix(Nodes, Edges, Matrix) :-
    diagonal_matrix(Nodes, DiagonalMatrix),
    direct_adjacencies(Edges, Nodes, DiagonalMatrix, DirectMatrix),
    adjacency_closure(DirectMatrix, Matrix).

diagonal_matrix(Nodes, Matrix) :-
    diagonal_matrix(Nodes, Nodes, Matrix).

diagonal_matrix([], _, []).
diagonal_matrix([H|T], Columns, [Row|OtherRows] ) :-
    diagonal_matrix1(Columns, H, Row),
    diagonal_matrix(T, Columns, OtherRows).

diagonal_matrix1([H|T], Reference, Row) :-
    H = Reference
      -> Row = [1|Zeros],
         diagonal_matrix2(T, Zeros)
    ;
    Row = [0|OtherFields],
    diagonal_matrix1(T, Reference, OtherFields).

diagonal_matrix2([], []).
diagonal_matrix2([_|T], [0|OtherFields]) :-
    diagonal_matrix2(T, OtherFields).

% direct_adjacencies(Edges, Nodes, MatrixIn, MatrixOut).

direct_adjacencies([], _Nodes, Matrix, Matrix).
direct_adjacencies([H|T], Nodes, MatrixIn, MatrixOut) :-
    direct_adjacencies1(H, Nodes, MatrixIn, MatrixNext),
    direct_adjacencies(T, Nodes, MatrixNext, MatrixOut).

direct_adjacencies1(edge(N1, N2), Nodes, MatrixIn, MatrixOut) :-
    nth1(Index1, Nodes, N1),
    nth1(Index2, Nodes, N2),
    update_matrix(MatrixIn, Index1, Index2, 1, MatrixOut).

update_matrix([RowIn|OtherRowsOut], 1, ColumnIndex, Value, [RowOut|OtherRowsOut]) :-
    !,
    update_matrix1(RowIn, ColumnIndex, Value, RowOut).
update_matrix([RowIn|OtherRowsIn], RowIndex, ColumnIndex, Value, [RowIn|OtherRowsOut]) :-
    RowIndex > 1,
    NextRowIndex is RowIndex - 1,
    update_matrix(OtherRowsIn, NextRowIndex, ColumnIndex, Value, OtherRowsOut).

update_matrix1([_FieldIn|OtherFieldsOut], 1, Value, [Value|OtherFieldsOut]) :-
    !.
update_matrix1([FieldIn|OtherFieldsIn], ColumnIndex, Value, [FieldIn|OtherFieldsOut]) :-
    ColumnIndex > 1,
    NextColumnIndex is ColumnIndex - 1,
    update_matrix1(OtherFieldsIn, NextColumnIndex, Value, OtherFieldsOut).

% matrix[i][j] = 1 and matrix[j][k] = 1 -> matrix[i][k] = 1.
% row1,colx rowx,col2, row1,col2
% iterate through matrix1 and matrix 3 in tandem, identifying each row1 and row3 pair.
% each m1 row: iterate through row1 and matrix2 in tandem.
% this identifies a row2 for each field of row1.
% for each m2 row: iterate through row2 and row3 in tandem.
% this identifies a field3 value.
% Continue the closure processing until there are no changes.
%
% adjacency_closure([[1, 1, 0], [0, 1, 1], [0, 0, 1]], M).

adjacency_closure(MIn, MOut) :-
    ac(MIn, MIn, MIn, MNext),
    (MIn = MNext
      -> MNext = MOut
    ;
    transpose(MNext, LowerMNext),
    union_adjacency(MNext, LowerMNext, FullMNext),
    adjacency_closure(FullMNext, MOut)
    ).

ac([], _, [], []).
ac([Row1|OtherRows1], M2, [Row3In|OtherRows3], [Row3Out|OtherRows3Out]) :-
    ac1(Row1, M2, Row3In, Row3Out),
    ac(OtherRows1, M2, OtherRows3, OtherRows3Out).

ac1([], [], Row3, Row3).
ac1([Col1|OtherCols1], [Row2|OtherRows2], Row3In, Row3Out) :-
    ac2(Row2, Col1, Row3In, Row3Next),
    ac1(OtherCols1, OtherRows2, Row3Next, Row3Out).

ac2([], _Value1AtIJ, [], []).
ac2([Value2AtJK|OtherCols2], Value1AtIJ, [Col3In|OtherCols3In], [Col3Out|OtherCols3Out]) :-
    (Value1AtIJ = 1,
     Value2AtJK = 1
       -> Col3Out = 1
    ;
     Col3Out = Col3In
    ),
    ac2(OtherCols2, Value1AtIJ, OtherCols3In, OtherCols3Out).

% transpose([[1,1,0,1],[0,1,0,1],[0,0,1,0],[0,0,0,1]], T).
transpose([], M) :-
    null(M).
transpose([Row|OtherRows], M) :-
    t(Row, M, MNext),
    transpose(OtherRows, MNext).

null([]).
null([[]|T]) :-
    null(T).

t([], [], []).
t([H|T], [[H|RowTail]|OtherRowsIn], [RowTail|OtherRowsOut]) :-
    t(T, OtherRowsIn, OtherRowsOut).

% M=[[1,1,0,1],[0,1,0,1],[0,0,1,0],[0,0,0,1]], transpose(M, MT), union_adjacency(M, MT, MU).

union_adjacency([], [], []).
union_adjacency([R1|M1], [R2|M2], [RU|Union]) :-
    union_adjacency1(R1, R2, RU),
    union_adjacency(M1, M2, Union).

union_adjacency1([], [], []).
union_adjacency1([H1|T1], [H2|T2], [HU|TU]) :-
    union_adjacency2(H1, H2, HU),
    union_adjacency1(T1, T2, TU).

union_adjacency2(0, 0, 0).
union_adjacency2(1, 0, 1).
union_adjacency2(0, 1, 1).
union_adjacency2(1, 1, 1).

% Nodes = [1,2,3,4], adjacency_matrix(Nodes, [edge(1,2), edge(2,4)], Matrix), components(Matrix, Nodes, Components)
components(Matrix, Nodes, Components) :-
    components(Matrix, Nodes, Nodes, RawComponents),
    sort(RawComponents, Components). % remove duplicates.

components([], [], _AllNodes, []).
components([Row|OtherRows], [_|OtherNodes], AllNodes, [Component|Components]) :-
    c(Row, AllNodes, Component),
    components(OtherRows, OtherNodes, AllNodes, Components).

c([], [], []).
c([Field|OtherFields], [Node|OtherNodes], Component) :-
    (Field = 1
      -> Component = [Node|OtherComponent]
    ;
    Component = OtherComponent
    ),
    c(OtherFields, OtherNodes, OtherComponent).
