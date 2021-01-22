candidate_number(51722).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ;part_module(4) ->find_identity_4(A)
  ; otherwise -> find_identity_o(A)
  ).

%%%%%%%%%% Part 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List all actors, then call recursive function
find_identity_2(A):-
  init_identity,
  findall(A, actor(A), Actors),
  find_actor(Actors, A),!.

% Base Case
find_actor(Actors, A):-
  Actors = [Actor],
  A = Actor.

% Repeatedly ask oracle for a link, eliminate actors which do not feature link
find_actor(Actors, A):-
  agent_ask_oracle(oscar,o(1),link,L),
  include([Actor]>>is_link_on_page(Actor, L), Actors, NewActors),
  find_actor(NewActors, A).

% Succeed if link features on the actor webpage
is_link_on_page(Actor, Link):-
   wp(Actor, WikiText),
   setof(WikiLink, wt_link(WikiText, WikiLink), WikiLinks),
   member(Link, WikiLinks).

% Succeed if link features on the actor webpage
is_link_on_page_o(Actor, Link):-
   wp(Actor, WikiText),
   setof(WikiLink, wt_link(WikiText, WikiLink), WikiLinks),
   member(Link, WikiLinks).

%%%%%%%%%% Part 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List all actors, find all oracles and charge stations, then call recursive function
find_identity_o(A):-
  init_identity,
  my_agent(Agent),
  query_world(agent_current_position, [Agent, P]),
  findall(A, actor(A), Actors),
  writeln('Finding oracles and stations...'),
  find_stuff([ ],[ ],[ ],[ ],[(-1,P)],[P],VO,VC),!,
  find_actor_o(Actors, A, VO, VC),!.

% Base Case
find_actor_o(Actors, A,_, _):-
  Actors = [Actor],
  A = Actor.

% Find actor first filters and sorts all possible oracles, then tests whether the cost
% of reaching a charging station from the closest oracle is larger than the current
% energy i.e. minimal charging path. If so, the agent tops up before continuing.

find_actor_o(Actors, A, VO, VC):-
  my_agent(Agent),
  query_world(agent_current_position, [Agent, P]),
  query_world(agent_current_energy, [Agent, E]),
  writeln('sorting oracles & stations...'),
  % Oracles
  filter_oracles(VO, P, 5, FiltO),!,
  sort_oracles(FiltO, [], VOSorted, P, E),!,
  VOSorted = [HeadO|_],
  HeadO = (CostO, PositionO, O), %Nearest Oracle
  % Charge Stations
  sort_oracles(VC, [ ], VCSorted, PositionO, E),!, %used to sort charge station based on distance to nearest oracle
  (VCSorted = [HeadC|_], HeadC = (CostC, _, _) %Nearest charge station to nearest oracle
  ;CostC is 0),
  (
  CostC + CostO + 10 > E ->
    choose_stat(VCSorted,Station), %Chooses best station to visit
    Station = (_,PosC,SC),
    writeln('-> topup'),
    solve_task(go(PosC), [cost(_),depth(_)]),!,
    query_world(agent_topup_energy, [Agent, SC]),
    find_actor_o(Actors, A, VO, VC)
  ; writeln('-> go to oracle'),
    solve_task(go(PositionO), [cost(_),depth(_)]),!,
    query_world(agent_ask_oracle, [Agent, O, link, L]),
    include([Actor]>>is_link_on_page_o(Actor, L), Actors, NewActors),
    find_actor_o(NewActors, A, VO, VC)
  ).

%%%%%%%%%% PART 4 Dynamic Search %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_identity_4(A) :-
  init_identity,
  findall(A, actor(A), Actors), 
  find_actor_4(Actors, A),!. %FIND ACTOR IDENTITY

find_actor_4(Actors, A  ):- %BASE CASE
  Actors = [Actor],
  A = Actor.

find_actor_4(Actors, A) :-
  writeln(Actors),
  my_agent(Agent),
  query_world(agent_current_position, [Agent, AgentPos]),
  query_world(agent_current_energy, [Agent, AgentEnergy]),
  (AgentEnergy < 40 -> %Check to refuel
  solve_task_bt(find(c(NC)),[c(0,0,AgentPos,AgentEnergy,[])],[AgentPos],RC,_,NewPos),!, %Find refuel station
  get_path(RC,RevC),!, %Get Reverse Path
  reverse(RevC,[_Init|PathC]),
  do_moves_c(PathC,Agent,c(NC),NewPos),!, %Iterate and move through each posistion in path
  find_actor_4(Actors, A) %Recurse
  ;solve_task_bt(find(o(N)),[c(0,0,AgentPos,AgentEnergy,[])],[AgentPos],R,_,NewPos1),\+ query_world(agent_check_oracle, [Agent,o(N)]),!, %Find unvisited oracle
  get_path(R,Rev),!,
  reverse(Rev,[_Init|Path]),
  do_moves(Path,Agent,o(N),NewPos1,L),!,
  include([Actor]>>is_link_on_page_o(Actor, L), Actors, NewActors),
  find_actor_4(NewActors,A) %Recurse
  ).


do_moves([],Agent,o(N),_,L):-
  query_world(agent_ask_oracle, [Agent, o(N), link, L]). %Base Case, query oracle

do_moves(Path,Agent,Task,NewPos,L):- 
  Path = [First|Rest],
  query_world(agent_current_position, [Agent, AgentPos]),
  query_world(agent_current_energy, [Agent, AgentEnergy]),
  (map_adjacent(AgentPos,First,empty),map_adjacent(NewPos,_,Task) -> query_world( agent_do_moves, [Agent,[First]] ), %Check cell to move into is empty and target hasnt moved
  do_moves(Rest,Agent,Task,NewPos,L),! %Recurse
  ;map_adjacent(_,NP,Task),!, %New pos of moved target
  solve_task_bt(go(NP),[c(0,0,AgentPos,AgentEnergy,[])],[AgentPos],R,_,NewPos1),!, %Find Path
  get_path(R,Rev),!,
  reverse(Rev,[_Init|Path1]),
  do_moves(Path1,Agent,Task,NewPos1,L),!
  ;solve_task_bt(find(o(NO)),[c(0,0,AgentPos,AgentEnergy,[])],[AgentPos],RO,_,NewPosO),\+ query_world(agent_check_oracle, [Agent,o(NO)]),!, %If path blocked find other target
  get_path(RO,RevO),!,
  reverse(RevO,[_Init|PathO]),
  do_moves(PathO,Agent,o(NO),NewPosO,L),!
  ).

%Same as for do_moves but for charge stations
do_moves_c([],Agent,c(N),_):-
  query_world(agent_topup_energy, [Agent, c(N)]).

do_moves_c(Path,Agent,Task,NewPos):- 
  Path = [First|Rest],
  query_world(agent_current_position, [Agent, AgentPos]),
  query_world(agent_current_energy, [Agent, AgentEnergy]),
  (map_adjacent(AgentPos,First,empty),map_adjacent(NewPos,_,Task) -> query_world( agent_do_moves, [Agent,[First]] ),
  do_moves_c(Rest,Agent,Task,NewPos),!
  ;map_adjacent(_,NP,Task),!,
  solve_task_bt(go(NP),[c(0,0,AgentPos,AgentEnergy,[])],[AgentPos],R,_,NewPos1),!,
  get_path(R,Rev),!,
  reverse(Rev,[_Init|Path1]),
  do_moves_c(Path1,Agent,Task,NewPos1),!
  ;solve_task_bt(find(c(NCO)),[c(0,0,AgentPos,AgentEnergy,[])],[AgentPos],RCO,_,NewPosO),!,
  get_path(RCO,RevCO),!,
  reverse(RevCO,[_Init|PathCO]),
  do_moves_c(PathCO,Agent,c(NCO),NewPosO),!
  ).

%%%%%%%%%% Oracle filtering functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Filterning Oracles works by first assessing whether they have already been visited,
% then whether they lie within a certain distance D from the Agent. If none exist
% within the D range, D is increased by 5 until it either finds a non-visited oracle
% or D exceeds the size of the board i.e. termination case. The less oracles we
% have to sort through, the faster the algorithm is at finding the next oracle to
% visit.


% Incresing D if no oracles are found within the distance.
filter_oracles(Oracles, P, D, F):-
 filter_oracles_r(Oracles, P, D, FiltO),
 (
 D < 200, FiltO = [] -> D1 is D + 5, filter_oracles(Oracles, P, D1, FilteredO), F = FilteredO;
 F = FiltO
 ).
filter_oracles_r([],_,_,[]).

% The actual filtering function
filter_oracles_r(Oracles, P, D, FilteredO) :-
 my_agent(Agent),
 Oracles = [First|Rest],
 First = (_,p(XO, YO),O),
 P = p(XP,YP),
 (
 % Checking whether the given oracle has been visited / lies within the give D range
 \+ query_world(agent_check_oracle, [Agent,O]),
 XO < XP + D, XO > XP - D, YO < YP + D, YO > YP - D
 -> FilteredO = [First|RestFilt] , filter_oracles_r(Rest, P, D, RestFilt)
 ;
 filter_oracles_r(Rest, P, D, FilteredO)
 ).

%%%%%%%%%% Oracle, charge stations sorting functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sorting oracles and charge stations by their cost to reach using A* search.

sort_oracles([],Temp,Temp,_,_).

sort_oracles(Oracles, Temp, NewOracles,AgentPos, AgentEnergy):-
  Oracles = [Current|Rest],
  Current = (_,OPos,T),
  solve_task_bt(go(OPos),[c(0,0,AgentPos,AgentEnergy,[])],[AgentPos],_,[cost(C),depth(_),energy(_)],_NewPos),!,
  insert_oracle((OPos,T), C, Temp, Temp2),
  sort_oracles(Rest,Temp2,NewOracles,AgentPos,AgentEnergy).

%insert to array based on cost 
insert_oracle((OPos,T), Cost, [], [(Cost,OPos,T)]).

insert_oracle(Current, Cost, Temp,Temp2) :-
  Temp = [First|Rest],
  First = (C,_,_),
  Current = (OPos,T),
  (Cost > C ->Temp2 = [First|NewRest], insert_oracle(Current,Cost,Rest,NewRest)
  ;Cost =< C -> Temp2 = [(Cost,OPos,T)|Temp]
  ).



%%%%%%%%%%%%%%%%%%%%SEARCH BOARD FUNCTIONS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BFS to locate positions and distances to oracles and charge stations

%Base Case
find_stuff(VisOracles,VisRecharge,_,_,[],_,NewVisO,NewVisR):-
  reverse(VisOracles,NewVisO),
  reverse(VisRecharge,NewVisR).

find_stuff(VisOracles,VisRecharge,TempVisO,TempVisR,Agenda,Visited,Oracles,Recharge):-
  Agenda = [Head|Rest],
  get_children(Head,Children,Visited),
  append(Rest,Children,NewAgenda),
  get_visited(Children, JustP),
  append(Visited,JustP,NewVisited),
  checkCurrent(Head,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR),
  find_stuff(NewVisO,NewVisR,PTVO,PTVR,NewAgenda,NewVisited,Oracles,Recharge).

% Check Current node for adjacent oracles and charge stations
checkCurrent(Head,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR):-
  Head = (Cost,Current),
  NewC is Cost +1,
  setof((NewC,Pos,O),map_adjacent(Current,Pos,O),Z),
  addstuff(Z,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR).

%Add found oracles and charge stations to Arrays

%Base Case
addstuff([],VisOracles,VisRecharge,VisOracles,VisRecharge,TempVisO,TempVisR,TempVisO,TempVisR).

%Add when oracles or recharge has not been visited
addstuff(Z,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR):-
  Z = [Head|Rest],
  Head = (_,P,T),
  (T = o(_) -> \+ member((P,T),TempVisO), addstuff(Rest,[Head|VisOracles],VisRecharge,NewVisO,NewVisR,[(P,T)|TempVisO],TempVisR,PTVO,PTVR)
  ;T = c(_) -> \+ member((P,T),TempVisR), addstuff(Rest,VisOracles,[Head|VisRecharge],NewVisO,NewVisR,TempVisO,[(P,T)|TempVisR],PTVO,PTVR)
  ;addstuff(Rest,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR)
  ).

%Dont add when oracle or charge station has been vivisted
addstuff(Z,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR):-
  Z = [Head|Rest],
  Head = (_,P,T),
  (T = o(_) -> member((P,T),TempVisO), addstuff(Rest,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR)
  ;T = c(_) -> member((P,T),TempVisR), addstuff(Rest,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR)
  ;addstuff(Rest,VisOracles,VisRecharge,NewVisO,NewVisR,TempVisO,TempVisR,PTVO,PTVR)
  ).

%Get Children nodes of the current node
get_children(Current,Children,Visited) :-
  Current = (Cost, P),
  NewC is Cost +1,
  setofsucc((NewC,C),map_adjacent(P,C,empty),Z),
  filter_children(Z,Children,Visited).

%Filter nodes bases on if node has been visited or not, returns non-visited children

filter_children([],[],_).

filter_children(Z,RestofChild,Visited) :-
  Z = [Head|Rest],
  Head = (_,First),
  member(First,Visited),
  filter_children(Rest,RestofChild,Visited).

filter_children(Z,[Head|RestofChild],Visited) :-
  Z = [Head|Rest],
  Head = (_,First),
  \+ member(First,Visited),
  filter_children(Rest,RestofChild,Visited).

%Get point-coordinate of children to add to visited array
get_visited([],[]).

get_visited(Children, [P|RestP]):-
  Children = [First|Rest],
  First = (_,P),
  get_visited(Rest,RestP).

% Used to determine the optimal charge station to visit in order to reach a desired
% oracle.
choose_stat(Statback,Station) :-
 my_agent(Agent),
 query_world( agent_current_position, [Agent,P] ),
 query_world( agent_current_energy, [Agent, Energy]),
 Statback = [First|Rest],
 First = (_,Pos,_),
 solve_task_bt(go(Pos),[c(0,0,P,Energy,[])],[P],_,[_,_,energy(ELeft)],_),!,
 (ELeft >= 0 -> Station = First
 ;choose_stat(Rest,Station) %Gets to closest station to oracle that can still be reached from current position
 ).

