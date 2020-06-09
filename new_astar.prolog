start_A_star( InitState, PathCost, N, Max) :-
 
    score(InitState, 0, 0, InitCost, InitScore) ,
 
    search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], PathCost, N,Max) .
 
 
 
 
search_A_star(Queue, ClosedSet, PathCost, N, Max) :-
 
    newFetch(Node, Queue, ClosedSet , RestQueue, N),
 
    continue(Node, RestQueue, ClosedSet, PathCost, N, Max).
 
 
 
continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet,
                            path_cost(Path, Cost), N, Max) :-
 
    N=<Max,goal( State), ! , 
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .

continue(Node, RestQueue, ClosedSet, Path, N, Max)   :-
 
    N>Max, write("Too Much").
 
 
continue(Node, RestQueue, ClosedSet, Path,N,Max)   :-
 	N=<Max,
    expand(Node, NewNodes),
 
    insert_new_nodes(NewNodes, RestQueue, NewQueue),
	N1 is N+1,
 
    search_A_star(NewQueue, [Node | ClosedSet ], Path, N1, Max).


newFetch(node(State, Action,Parent, Cost, Score), [node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet,  RestQueue, N) :-
    N>=1,\+ member(node(State, _ ,_  , _ , _ ) , ClosedSet).


newFetch(Node, [node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet,  RQueue, N) :-
	N>=1,member(node(State, _ ,_  , _ , _ ), ClosedSet), 
	newFetch(Node, RestQueue, ClosedSet , RQueue,N).


newFetch(Node, [node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet,  [node(State, Action,Parent, Cost, Score) | NewQueue], N) :-
	N>1,N1 is N-1,newFetch(Node, RestQueue, ClosedSet , NewQueue,N1).
 
 
fetch(node(State, Action,Parent, Cost, Score),
            [node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet,  RestQueue) :-
 
    \+ member(node(State, _ ,_  , _ , _ ) , ClosedSet), !.
 
 
fetch(Node, [ _ |RestQueue], ClosedSet, NewRest) :-
 
    fetch(Node, RestQueue, ClosedSet , NewRest).
 
 
expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-
 
    findall(node(ChildState, Action, State, NewCost, ChildScore) ,
            (succ(State, Action, StepCost, ChildState),
             score(ChildState, Cost, StepCost, NewCost, ChildScore) ) ,
 			 NewNodes) .
 
 
score(State, ParentCost, StepCost, Cost, FScore)  :-
 
    Cost is ParentCost + StepCost ,
 
    hScore(State, HScore),
 
    FScore is Cost + HScore .


insert_new_nodes( [ ], Queue, Queue) .
 
insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-
 
    insert_p_queue(Node, Queue, Queue1),
 
    insert_new_nodes( RestNodes, Queue1, NewQueue) .


insert_p_queue(Node,  [ ], [Node] )  :-  ! .
 
 
insert_p_queue(node(State, Action, Parent, Cost, FScore),
        [node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
            [node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-
 
    FScore >= FScore1,  ! ,
 
    insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1) .
 
 
insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
                [node(State, Action, Parent, Cost, FScore)|Queue]) .
 
 
 
 
 
 
 
 
build_path(node(nil, _, _, _, _ ), _, Path, Path) :-    ! .
 
build_path(node(EndState, _ , _ , _, _ ), Nodes, PartialPath, Path)  :-
 
    del(Nodes, node(EndState, Action, Parent , _ , _  ) , Nodes1) ,
 
    build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1,
                        [Action/EndState|PartialPath],Path) .
 
 
del([X|R],X,R).
del([Y|R],X,[Y|R1]) :-
    X\=Y,
    del(R,X,R1).
