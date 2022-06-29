:- dynamic([hunterAt/2,
            room/2,
            pit/1,
            wumpus/1
           ]).

% Reseting the game.

:- random(1,5,X), random(1,5,Y),
   \+hunterAt(r(X,Y), _),
   retractall(wumpus(r(_,_))),assert(wumpus(r(X,Y))).

% Adding rooms to the KB.

:- retractall(room(_,_)),
   forall(member(X-Y, [1-1,1-2,1-3,1-4,2-1,2-2,2-3,2-4,3-1,3-2,3-3,3-4,4-1,4-2,4-3,4-4]),
   assert(room(X,Y))).

% Initializing the game based on user input.

start :- setHunterAt(),
         setPitsPos(),
         setWumpusPos(),
         setGoldPos().

setHunterAt() :- (retractall(hunterAt(r(_,_),_)),
                 write('Enter hunter X coordinate: '), nl,read(X),
                 write('Enter hunter Y coordinate: '), nl, read(Y),
                 write('Enter hunter facing position: '), nl, read(Z),
                 assert(hunterAt(r(X,Y),Z))).

% Setting pits with a probability of 25%.

setPitsPos() :- retractall(pit(r(_,_))), pitsPos().
pitsPos() :- forall(member(X-Y, [1-1,1-2,1-3,1-4,2-1,2-2,2-3,2-4,3-1,3-2,3-3,3-4,4-1,4-2,4-3,4-4]), pitProb(X,Y)).
pitProb(X,Y):- (maybe(0.25),\+hunterAt(r(X,Y),_),\+wumpus(r(X,Y)) ,assert(pit(r(X,Y)))); true.

% Setting wumpus position.

setWumpusPos() :- \+wumpusPos() -> setWumpusPos(); true.
wumpusPos() :- random(1,5,X), random(1,5,Y),
                     \+hunterAt(r(X,Y),_),\+pit(r(X,Y)),
                     retractall(wumpus(r(_,_))),assert(wumpus(r(X,Y))).

% Setting gold position.

setGoldPos():- \+goldPos() -> setGoldPos(); true.
goldPos() :- random(1,5,X), random(1,5,Y),
               \+hunterAt(r(X,Y),_),\+pit(r(X,Y)),
               retractall(gold(r(_,_))),assert(gold(r(X,Y))).

% Adjacent rooms of a given room.

getAdjacentRooms(r(X,Y),L) :-
    XL is X-1,
    XR is X+1,
    YD is Y-1,
    YU is Y+1,
    append([XL-Y, XR-Y, X-YU, X-YD],[],L).

% Checking if rooms are adjacent.

adjacentTo(r(X,Y), r(Z,A)) :-  ((abs(X-Z) =:= 1 , Y =:= A); (abs(Y-A) =:= 1 , X =:= Z)).

% Breeze check.

breeze(r(X,Y)) :- room(X,Y), pit(r(Z,A)),
               adjacentTo(r(X,Y) , r(Z,A)).

% Stench check.

stench(r(X,Y)) :- room(X,Y), wumpus(r(Z,A)), adjacentTo(r(X,Y), r(Z,A)).

% Safety check.

safeRoomCheck(r(X,Y)) :- (room(X,Y),\+pit(r(X,Y)), \+wumpus(r(X,Y)), format('Room at X: ~d, Y:~d is safe\n', [X,Y])); true.

% Check if hunter's adjacent rooms are safe.

safe() :- hunterAt(r(X,Y),_), getAdjacentRooms(r(X,Y),L), forall(member(Z-A, L), safeRoomCheck(r(Z,A))).
safeWumpus() :- hunterAt(r(X,Y),_), wumpus(r(Z,A)), \+adjacentTo(r(X,Y),r(Z,A)).
wumpusAlive() :-  wumpus(r(_,_)).
hasArrow() :- wumpusAlive().

% Walls check.

wallCheck(r(X,Y)) :- X < 1 ; X > 4; Y < 1 ; Y>4.

% Shoot wumpus.

shootWumpus() :- hunterAt(r(X,Y),_), getAdjacentRooms(r(X,Y),L),
                \+(foreach(member(Z-A, L), \+wumpus(r(Z,A)))),
                 retractall(wumpus(r(_,_))).

% Shoot while taking hunter direction into consideration.

shootWumpusWithDirection() :-  hunterAt(r(X,Y),D), getAdjacentRooms(r(X,Y),L),
                               \+(foreach(member(Z-A, L), \+wumpus(r(Z,A)))), wumpus(r(F,E)),
                               ((X-F =:= 1, D=w);(X-F =:= -1, D=e);(Y-E =:= 1, D=s);(Y-E =:= -1, D=n)),
                                retractall(wumpus(r(_,_))).

% Grab gold if possible.

grabGold():- hunterAt(r(X,Y),_), gold(r(X,Y)), retractall(gold(r(_,_))).

% Turn hunter left.

turnLeft() :- hunterAt(r(X,Y),Z),
    retractall(hunterAt(r(X,Y),Z)),
    ((Z = n, assert(hunterAt(r(X,Y),w)));
    (Z = e,  assert(hunterAt(r(X,Y),n)));
    (Z = s, assert(hunterAt(r(X,Y),e)) );
    (Z = w, assert(hunterAt(r(X,Y),s))) ).

% Turn hunter right.

turnRight() :- hunterAt(r(X,Y),Z),
    retractall(hunterAt(r(X,Y),Z)),
    ((Z = n, assert(hunterAt(r(X,Y),e)));
    (Z = e,  assert(hunterAt(r(X,Y),s)));
    (Z = s, assert(hunterAt(r(X,Y),w)) );
    (Z = w, assert(hunterAt(r(X,Y),n))) ).