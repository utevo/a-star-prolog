start_A_star( InitState, PathCost, N, MaxStep) :-
 
    score(InitState, 0, 0, InitCost, InitScore) ,
 
    search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], PathCost, N, 0, MaxStep) .
 
 
 
 
search_A_star(Queue, ClosedSet, PathCost, N, Step, MaxStep) :-
 
    writeStep(Step),

    newFetch(Node, Queue, ClosedSet , RestQueue, N),
 
    continue(Node, RestQueue, ClosedSet, PathCost, N, Step, MaxStep).
 
 
 
continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet,
                            path_cost(Path, Cost), N, Step, MaxStep) :-
 
    Step=<MaxStep, goal(State), ! , 
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .

continue(Node, RestQueue, ClosedSet, Path, N, Step, MaxStep)   :-
 
    Step>MaxStep, write("Too much steps").
 
 
continue(Node, RestQueue, ClosedSet, Path, N, Step, MaxStep)   :-
 	Step=<MaxStep,
    expand(Node, NewNodes),
 
    insert_new_nodes(NewNodes, RestQueue, NewQueue),

	NextStep is Step+1,
 
    search_A_star(NewQueue, [Node | ClosedSet ], Path, N, NextStep, MaxStep).

% my_poligon

writeStep(Step) :-
    format('Step nr: ~w', [Step]), nl.

readFirstN(Nodes, Queue, ClosedSet, N) :-
    findall(Node, newFetch(Node, Queue, ClosedSet, _, N), Nodes) .

% fetchN(Node, [node(a,nil,nil,0,0),node(b,nil,nil,0,0),node(c,nil,nil,0,0)], [], NewQueue, 2).
fetchN(node(State, Action,Parent, Cost, Score), [node(State, Action,Parent, Cost, Score) | RestQueue], ClosedSet, RestQueue, 1) :-
    \+ member(node(State, _, _, _, _) , ClosedSet).

fetchN(Node, [node(State, Action,Parent, Cost, Score) | RestQueue], ClosedSet, [node(State, Action,Parent, Cost, Score) | NewQueue], N) :-
    N>1,
    \+ member(node(State, _, _, _, _) , ClosedSet),

    N1 is N - 1,
    fetchN(Node, RestQueue, ClosedSet, NewQueue, N1).

fetchN(Node, [node(State, Action,Parent, Cost, Score) | RestQueue], ClosedSet, [node(State, Action,Parent, Cost, Score) | NewQueue], N) :-
    N>1,
    member(node(State, _, _, _, _) , ClosedSet),

    fetchN(Node, RestQueue, ClosedSet, NewQueue, N).

% fetchWithOrder(Node, [2, 1], [node(a,nil,nil,0,0),node(b,nil,nil,0,0),node(c,nil,nil,0,0)], [], NewQueue).
fetchWithOrder(Node, [N | _], Queue, ClosedSet, NewQueue) :-
    fetchN(Node, Queue, ClosedSet, NewQueue, N).

fetchWithOrder(Node, [_ | RestOrder], Queue, ClosedSet, NewQueue) :-
    fetchWithOrder(Node, RestOrder, Queue, ClosedSet, NewQueue).

% globalFetch(Node, [node(a,nil,nil,0,0),node(b,nil,nil,0,0),node(c,nil,nil,0,0)],[], 2).
newFetch(Node, Queue, ClosedSet, NewQueue, N) :-
    readFirstN(Nodes, Queue, ClosedSet, N),
    write(Nodes), nl,
    read(Order),
    fetchWithOrder(Node, Order, Queue, ClosedSet, NewQueue).

% end my_poligon

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
