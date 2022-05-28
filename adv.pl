:- use_module(library(lists)).
:- use_module(library(assoc)).

%:- initialization main.

/*

           ┌────┐  ┌────┐
           │ R0 │  │ R4 │
           └─┬┬─┘  └─┬┬─┘
             ││      ││
   ┌────┐  ┌─┴┴─┐  ┌─┴┴─┐
   │ R2 ├──┤ R1 ├──┤ R3 │
   └────┘  └─┬┬─┘  └────┘
             ││
           ┌─┴┴─┐
           │ R5 │
           └────┘

*/

direction(north).
direction(east).
direction(south).
direction(west).

opposite_direction(south, north).
opposite_direction(north, south).
opposite_direction(east, west).
opposite_direction(west, east).

room(r0).
room(r1).
room(r2).
room(r3).
room(r4).
room(r5).

door(r0, r1, south).
door(r1, r2, west).
door(r1, r3, east).
door(r3, r4, north).
door(r1, r5, south).

description("Looks like a cell. Smells like a cell. Chances are high you are in a cell.", r0).
description("This was probably the guard room.", r1).
description("Pillar riddle.", r2).
description("Smt. smt to open next door", r3).
description("Fight and loot, use a switch or smt to open door to r5", r4).
description("You found an exit. You free now. Are you?").


% ---------------------------------------------------------------%
%                          movement
% ---------------------------------------------------------------%

% query/mutate state
position(State, Room) :-
    get_assoc(position, State, Room).

change_position(State, NewPosition, NewState) :-
    put_assoc(position, State, NewPosition, NewState).

% constraints
connect(X,Y,D) :- door(X,Y,D).
connect(X,Y,D) :- opposite_direction(D,O), door(Y,X,O).

can_go(State, Destination) :-
    position(State, Current_location),
    connect(Current_location, Destination, _).

can_go(_, _) :- write("You can't go there"), nl, fail.

% view / user interaction
list_connections(Place) :-
    connect(Place, _, D),
    tab(2),
    write(D),
    nl,
    fail.

list_connections(_).

look(State) :-
    position(State, X), description(Desc, X),
    write(Desc), nl,
    write("You can go to:"),nl,
    list_connections(X).

goto(State, NewPosition, NewState) :-
    change_position(State, NewPosition, NewState),
    look(NewState).


% ---------------------------------------------------------------%
%                        items/inventory
% ---------------------------------------------------------------%

% query/mutate state
inventory(State, Inventory) :-
    get_assoc(inventory, State, Inventory).

item_location(State, Items) :-
    get_assoc(items, State, Items).

item_in_room(State, Room, Item) :-
    get_assoc(items, State, ItemsInRoom),
    member(Room/Item, ItemsInRoom).

take_item(State, Item, NewState) :-
    position(State, Room),
    item_location(State, ItemLocations),
    select(Room/Item, ItemLocations, NewItemLocations),
    inventory(State, Inventory),
    NewInventory = [Item | Inventory],
    put_assoc(inventory, State, NewInventory, TmpState),
    put_assoc(items, TmpState, NewItemLocations, NewState).

drop_item(State, Item, NewState) :-
    inventory(State, Inventory),
    select(Item, Inventory, NewInventory), % handle item does not exists case
    put_assoc(inventory, State, NewInventory, NewState).

drop_item(State, _, State).

% constraints
can_take(State, Item) :-
    position(State, Position),
    item_in_room(State, Position, Item).

can_take(_, _) :- write("This seems wired."), nl, fail.

% view / user interaction
list_inventory(State) :-
    write("Inventory:"),
    nl,
    inventory(State, X),
    tab(2),
    write(X),
    nl,
    fail.

take(State, Item, NewState) :-
    write("You put: "), write(Item), write(" into your bag"), nl,
    take_item(State, Item, NewState).

r1_puzzle(State) :-
    inventory(State, Inventory),
    member(torch, Inventory).

r1_puzzle(_) :- write("It's too dark. You can't see anything."), nl, fail.


% ---------------------------------------------------------------%
%                          pillars
% ---------------------------------------------------------------%

% query/mutate state
pillars(State, Pillars) :-
    get_assoc(pillars, State, Pillars).

update_pillars(State, Pillars, NewState) :-
    put_assoc(pillars, State, Pillars, NewState).

% constraints
can_touch_pillar(State) :-
    position(State, Room),
    Room = r2.

can_touch_piller(_) :- write("You can't do that."), nl, fail.

% test data
pillars([[snake, bear, eagle],
         [bear, eagle, snake],
         [eagle, snake, bear]]).

% mechanics
rotate([H|T],R) :- append(T, [H], R).

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   nth0(Index,List,OldElem,Transfer),
   nth0(Index,NewList,NewElem,Transfer).

rotate_pillar(Pillars, Index, NewPillars) :-
    replace_nth0(Pillars, Index, ToRotate, Rotated, NewPillars),
    rotate(ToRotate, Rotated).

% view / user interaction
list_pillars([[A|_], [B|_], [C|_]]) :-
    write(A), write(", "),
    write(B), write(", "),
    write(C), nl.

interact(State, pillar, Index, NewState) :-
    write("The pillar rotates."), nl,
    pillars(State, Pillars),
    rotate_pillar(Pillars, Index, NewPillars),
    update_pillars(State, NewPillars, NewState),
    list_pillars(NewPillars).

r3_puzzle(State) :-
    pillars(State, Pillars),
    Pillars = [[bear, eagle, snake],
               [snake, bear, eagle],
               [eagle, snake, bear]].

r3_puzzle(_) :-
    write("The door is locked. "),
    nl, nl,
    write("There is something on the door ..."), nl,
    fail.


% ---------------------------------------------------------------%
%                        end conditions
% ---------------------------------------------------------------%

quit(State, NewState) :-
    put_assoc(end, State, quit, NewState).


% ---------------------------------------------------------------%
%                           game loop
% ---------------------------------------------------------------%

constraint(State, goto(r1)) :- !, r1_puzzle(State).
constraint(State, goto(r3)) :- !, r3_puzzle(State).
constraint(State, goto(Direction)) :- !, can_go(State, Direction).
constraint(State, take(Item)) :- !, can_take(State, Item).
constraint(State, interact(pillar, _)) :- !, can_touch_pillar(State).
constraint(_, _).

do(State, State, look) :- look(State), !.
do(State, NewState, goto(NewPosition)) :- goto(State, NewPosition, NewState), !.
do(State, NewState, quit) :- quit(State, NewState), !.
do(State, NewState, take(Item)) :- take(State, Item, NewState), !.
do(State, State, i) :- list_inventory(State), !.
do(State, NewState, interact(pillar, Index)) :- interact(State, pillar, Index, NewState).
do(State, State, _) :-
    write("illegal command"), nl.

end_msg(quit) :- write("bye").
end_msg(win) :- write("gz you won").

main_loop(State) :-
    get_assoc(end, State, Value),
    end_msg(Value).

main_loop(State) :-
    repeat,
    write('> '),
    read(X),
    constraint(State, X),
    do(State, NewState, X),
    main_loop(NewState).

init(State) :-
    list_to_assoc([position-r0,
                   inventory-[],
                   items-[r0/torch],
                   pillars-[[snake, bear, eagle],
                            [bear, eagle, snake],
                            [eagle, snake, bear]]], State).

start :-
    write("You wake up ..."), nl,
    init(State),
    main_loop(State).

