candidate_number(51722).

solve_task(Task,[cost(C),depth(D)]):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  query_world( agent_current_energy, [Agent, Energy]),
  solve_task_bt(Task,[c(0,0,P,Energy,[])],[P],R,[cost(C),depth(D),energy(_)],_NewPos),!,  % prune choice point for efficiency
  get_path(R,Rev),
  reverse(Rev,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% A* Search %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_task_bt(Task,Current,_,RPath,Cost,NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos). %Check if current node is the required destination

solve_task_bt(Task,Agenda,Visited,RR,Cost,NewPos) :-
  Agenda = [Head|Rest],
  search(Head,L,Task,Visited,NewVis), % Search for children and filter
  insert_agenda(Rest,L,NewAgenda),
  append(Visited,NewVis,CompleteVis),
  solve_task_bt(Task,NewAgenda,CompleteVis,RR,Cost,NewPos).  % backtrack search, add routes to agenda

% Satisfied when Exit is reached, now also accepts positions next to oracles and
% charge stations if their positions are Exit, helps with A* search in part 3.
achieved(go(Exit),Current,Head,[cost(Cost),depth(Depth),energy(ELeft)],NewPos) :-
  Current = [Head|_],
  Head = c(Cost,Depth,NewPos,ELeft,_),
  ( Exit=none -> true
  ; map_adjacent(NewPos,Exit, o(_)) -> true
  ; map_adjacent(NewPos,Exit, c(_)) -> true
  ; NewPos = Exit -> true
  ).

% Satisfied when adjacent to O, if O = oracle, check if oracle has been visited
achieved(find(O),Current,Head,[cost(Cost),depth(Depth),energy(ELeft)],NewPos) :-
  my_agent(Agent),
  Current = [Head|_],
  Head = c(Cost,Depth,NewPos,ELeft,_),
  map_adjacent(NewPos,_,O),
  (O = c(_) -> true
  ; O = o(N) -> \+ query_world(agent_check_oracle, [Agent, o(N)])
  ).

search(Term,L,Task,Visited,NewVis) :-
  Term = c(_,_,P,_,_),
  my_agent(Agent),
  query_world(agent_current_position,[Agent,Pos]),
  setofsucc(C,map_adjacent(P,C,empty),Z),
  % if dist between P and agentpos = 1 add to Z
  (map_distance(Pos,P,1) -> append([Pos],Z,ZN)
    ;append([],Z,ZN)
  ),
  dist(Term,Task,ZN,[],L,Visited,NewVis).

% Makes sure setof does not fail
setofsucc(Goal,Task,Arr) :-
  (
    setof(Goal,Task,Arr) -> true
    ; Arr = []
  ).

%%%%% Calculate heuristics %%%%%%%%%%%%%%

dist(_,_,[],R,L,_,[]) :-
  append(R,[],L).

dist(Term,Task,Z,R,L,Visited,[First|NewVis]) :-
  Z = [First|Rest],
  Term = c(S,Depth,_,ELeft,_),
  (Task = go(Exit) -> map_distance(Exit,First,V), V1 is V+1+Depth %% A* Search Heuristic
  ; V1 is S+1 %%Otherwise BFS heuristic
  ),
  \+memberchk(First,Visited), % Make sure to get rid of visited nodes
  D is Depth +1,
  E is ELeft -1, %Energy Left
  dist(Term,Task,Rest,[c(V1,D,First,E,Term)|R],L,Visited,NewVis).

dist(Term,Task,Z,R,L,Visited,NewVis) :-
  Z = [First|Rest],
  memberchk(First,Visited),
  dist(Term,Task,Rest,R,L,Visited,NewVis).

%%%%%% Insert Items into Agenda %%%%%%%%%

insert_agenda(Agenda,[],Agenda).

insert_agenda(OldAgenda,[Child|Children],NewAgenda):-
  insert_one(Child,OldAgenda,TmpAgenda),
  insert_agenda(TmpAgenda,Children,NewAgenda).

% Finds where to insert one item into the agenda
insert_one(Child,[],[Child]).

insert_one(Child,[Node|Rest],NewAgenda):-
  Node = c(S,_,P1,_,_),
  Child = c(S1,_,P2,_,_),
  S1<S,
  (P1 \= P2 -> NewAgenda = [Child,Node|Rest]  %Also gets rid of visited nodes
  ; NewAgenda = [Node|Rest]
  ).

insert_one(Child,[Node|Rest],[Node|NewRest]):-
  Node = c(S,_,P1,_,_),
  Child = c(S1,_,P2,_,_),
  S1 >= S,
  (P1 \= P2 -> insert_one(Child,Rest,NewRest)
  ; NewRest = Rest
  ).


%%%%%%%% Construct point path %%%%%%%%%%
get_path([],[]).

get_path(Struct,[P|Path]):-
  Struct = c(_,_,P,_,R),
  get_path(R,Path).
