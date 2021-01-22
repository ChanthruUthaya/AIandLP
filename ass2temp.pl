candidate_number(51722).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*

solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).
  
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(F,P)|RPath],
  Task = go(Exit),
  search(P,P1,R,C),
  %search(P,L,C),
  \+memberchk(R,RPath),
  %add_filt(L,RPath,[P1|Rest]),
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_] 
  ).

achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
search(F,L,1) :-
  setof(C,map_adjacent(F,C,empty),L).

add_filt([],Visited,Filterd).
add_filt([Child|Rest],Visited,[Child|Filterd]):-
  \+ memberchk(Child,Visited),
  add_filt(Rest,Visited,Filterd).
add_filt([Child|Rest],Visited,Filterd):-
  memberchk(Child,Visited),
  add_filt(Rest,Visited,Filterd).







(E< 30 -> get_runout(R,X),
  find_nearest(X,(N,EPath)),
  reverse(EPath,[_Init|Path1]),
  query_world( agent_do_moves, [Agent,Path1] ),
  query_world( agent_topup_energy, [Agent,c(N)])
  ;get_path(R,Rev),
  reverse(Rev,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] )),
  X is C,
  Y is D.



  (
  CostOC + 10 > E ->
    writeln('topup'),
    solve_task(go(PositionC), [cost(_),depth(_)]),
    query_world(agent_topup_energy, [Agent, C]),
    find_actor_o(Actors, A, VO, VC)
  ; otherwise ->
      writeln('go to oracle'),
    solve_task(go(PositionO), [cost(_),depth(_)]),
      writeln('task solved'),
    query_world(agent_ask_oracle, [Agent, O, link, L]),
    include([Actor]>>is_link_on_page_o(Actor, L), Actors, NewActors),
    find_actor_o(NewActors, A, RestO, VC)
  ).


  choose_stat([],(S,R),_):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(find(c(S)),[c(0,0,P,_,[])],[P],R,_,_).