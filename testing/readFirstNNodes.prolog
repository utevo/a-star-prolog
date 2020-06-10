% readFirstNNodes
readFirstNNodes(Nodes, Queue, ClosedSet, N) :-
    findall(Node, newFetch(Node, Queue, ClosedSet, _, N), Nodes) .

% test readFirstNNodes
% readFirstNNodes(Nodes, [node(a,nil,nil,0,0),node(b,nil,nil,0,0),node(c,nil,nil,0,0)],[], 2).

findall(Node, newFetch(Node, [node(a,nil,nil,0,0),node(b,nil,nil,0,0),node(c,nil,nil,0,0)], [], _, 2), Nodes) .