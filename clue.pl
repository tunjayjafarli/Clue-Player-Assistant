/* Clue Player Assistant */


/*
 * DEFINE VALID CARDS
 */

% Set of suspects
suspect(scarlet).
suspect(plum).
suspect(peacock).
suspect(green).
suspect(mustard).
suspect(white).

% Set of weapons
weapon(knife).
weapon(candlestick).
weapon(pistol).
weapon(rope).
weapon(bat).
weapon(ax).

% Set of rooms
room(kitchen).
room(patio).
room(spa).
room(theatre).
room(livingroom).
room(observatory).
room(hall).
room(guesthouse).
room(diningroom).

% Define the 'card/1' predicate
card(X) :- suspect(X).
card(X) :- room(X).
card(X) :- weapon(X).


/*
 * KEEP TRACK OF PLAYERS
 */

% Valid players
validplayeramount(2).
validplayeramount(3).
validplayeramount(4).
validplayeramount(5).
validplayeramount(6).

% Players
next(player1,player2).
next(player2,player3) :- player(player3).
next(player3,player4) :- player(player4).
next(player4,player5) :- player(player5).
next(player5,player6) :- player(player6).
next(player6,player1) :- db_legalplayeramount(6).
next(player5,player1) :- db_legalplayeramount(5).
next(player4,player1) :- db_legalplayeramount(4).
next(player3,player1) :- db_legalplayeramount(3).
next(player2,player1) :- db_legalplayeramount(2).

player(player1).
player(player2).
player(player3) :- db_legalplayeramount(3).
player(player3) :- db_legalplayeramount(4).
player(player3) :- db_legalplayeramount(5).
player(player3) :- db_legalplayeramount(6).

player(player4) :- db_legalplayeramount(4).
player(player4) :- db_legalplayeramount(5).
player(player4) :- db_legalplayeramount(6).

player(player5) :- db_legalplayeramount(5).
player(player5) :- db_legalplayeramount(6).
player(player6) :- db_legalplayeramount(6).

:- dynamic db_legalplayeramount/1, db_playername/2, db_myplayer/1, db_failedplayer/1.


/*
 * EXCEPTION HANDLING
 */

start :- catch(startgame,gameover,(nl,write('Game over!'),nl)).
startgame :- initgame,write('Please be patient while I make the best suggestion for you...'),nl,nl,playbegins(player1).

/*
 * INITIALIZE THE GAME SETUP
 */

initgame :- startmessage,countplayers,assignplayers,nl,showcards,recordmycards,!,write('Game initialized! Now we\'re all set to start playing!'),nl.

% Print all the cards in the game
showcards :- write('Here are all the possible cards in the game: '),nl,nl,
             listing(suspect),listing(weapon),listing(room),
             write('NOTE: when entering cards, do NOT include their types.'),nl,nl.

% Print the welcome message
startmessage :- write('Welcome to The Clue Player Assistant!'),nl,nl,
                write('All inputs should be in lowercase.'),nl,nl.

% Sets the total number of players in the game
countplayers :- write('Enter the number of players in the game: '),nl,
                read(_N),validplayeramount(_N),assert(db_legalplayeramount(_N));
                write('Invalid input. Please enter a number between 2 and 6.'),nl,countplayers.

% Start assigning players with the first player
% Assumes that there is always more than one player (The check for this is done in 'countplayers')
assignplayers :- write('Enter the name of the first player: '),nl,
                  read(_P1),assert(db_playername(player1,_P1)),assignplayer2.

% Sets the second player
% If there are only 2 players, then set my own player; otherwise set the third player
assignplayer2 :-  write('Enter the name of the second player: '),nl,
                  read(_P2),assert(db_playername(player2,_P2)),(db_legalplayeramount(2) -> assignmyplayer; assignplayer3).

% Sets the third player
% If there are only 3 players, then set my own player; otherwise set the fourth player
assignplayer3 :- write('Enter the name of the third player: '),nl,
                  read(_P3),assert(db_playername(player3,_P3)),(db_legalplayeramount(3) -> assignmyplayer; assignplayer4).

% Sets the fourth player
% If there are only 4 players, then set my own player; otherwise set the fifth player
assignplayer4 :- write('Enter the name of the fourth player: '),nl,
                  read(_P4),assert(db_playername(player4,_P4)),(db_legalplayeramount(4) -> assignmyplayer; assignplayer5).

% Sets the fifth player
% If there are only 5 players, then set my own player; otherwise set the sixth player
assignplayer5 :- write('Enter the name of the fifth player: '),nl,
                  read(_P5),assert(db_playername(player5,_P5)),(db_legalplayeramount(5) -> assignmyplayer; assignplayer6).

% Sets the sixth player
% Assumes there are no more than 6 players, and sets my own player. (Checking for this is done in 'countplayers')
assignplayer6 :- write('Enter the name of the sixth player: '),nl,
                  read(_P6),assert(db_playername(player6,_P6)),assignmyplayer.


% Sets my own player to be one of the player names
assignmyplayer :- write('Which player are you?'),nl,
               read(_MP),player(X),db_playername(X,_MP),assert(db_myplayer(X));
               write('Invalid player name. Please check your spelling and retry!'),nl,assignmyplayer.

% Sets my own cards to db_mycard
recordmycards :- write('Enter a card from your hand.'),nl,
              read(_C),card(_C),!,assert(db_mycard(_C)),recordnextcard;
              write('Invalid card. Please check your spelling and try again!'),nl,recordmycards.
% Check if I have more cards.
recordnextcard :- write('Do you have any more cards? Enter "y"/"n".'),nl,
               read(_A),gotmorecards(_A),!;
               write('Invalid answer! Please answer with either "y." or "n."'),nl,recordnextcard.

% If you have any other card(s) to input, answer with "y"; otherwise "n"
gotmorecards(X) :- X = n,!.
gotmorecards(X) :- X = y,!,recordmycards.


/*
 * GAME PLAY
 */

playbegins(X) :- checkturn(X),next(X,Y),playbegins(Y).

% Check whose turn it is. Call my turn if it is db_myplayer; otherwise it's other player's turn
checkturn(X) :- not(db_myplayer(X)),!,othersturn(X).
checkturn(X) :- db_myplayer(X),!,myturn(X).

% If it's other's turn, check if the player already failed;
% Otherwise, check and if someone 'NotMe' suggested or accused
othersturn(X) :- db_failedplayer(X),!.
othersturn(X) :- not(db_failedplayer(X)),!,db_playername(X,NotMe),
                 write('It is '),write(NotMe),write('\'s turn. Did '),write(NotMe),write(' suggest or accuse?'),nl,
                 read(_A),suggestoraccuse(_A,X),!;
                 write('Invalid input! Try again.'),nl,othersturn(X).

% Helper to call other player's accuse or suggest according to input
suggestoraccuse(R,X) :- R = accuse,!,accusebyothers(X).
suggestoraccuse(R,X) :- R = suggest,!,suggestbyothers(X).

% Inform the db_myplayer about my move(accuse or suggest), the suspect, weapon, and room.
% Lets myjudgehelper check and handle for accuse or suggest. 
myturn(X) :-  mymovehelper([M,S,W,R]),write('This is your turn. You should '),write(M),write(' that: '),nl,
              write(S),write(' did it, '),
              write('with the '),write(W),write(','),
              write(' in the '),write(R),write('.'),nl,nl,
              myjudgehelper(X,M,[S,W,R]).

% Game ends if accusation is made; otherwise, go to next turn and trydisproveme
myjudgehelper(X,accuse,S) :- throw(gameover).
myjudgehelper(X,suggest,S) :- next(X,Y), trydisproveme(S,Y).

% Check if another player has passed or showed a card to disprove suggestion.
trydisproveme(S,Y) :- db_myplayer(Y),!.
trydisproveme(S,Y) :- db_playername(Y,Name),write('Did '),write(Name),write(' "pass" or "show"?'),nl,
                       read(_A),passorshowme(_A,S,Y);
                       write('Enter either "pass" or "show".'),trydisprove(X,S,Y).

% Either pass or show me the card to disprove my suggestion
passorshowme(R,S,Y) :- R = pass,!,next(Y,Z), assert(db_passed(Y,S)), trydisprove(X,S,Z).
passorshowme(R,S,Y) :- R = show,!,showcardtome(Y).

% Store the card shown to me that disproved my suggestion and store it into database db_cardsshownme
showcardtome(Y) :- write('Enter the shown disproving card: '),nl,
                   read(_C),card(_C),!,assert(db_cardsshownme(Y,_C));
                   write('Invalid card! Try again. '),nl,showcardtome(Y).

% Read another player's suggestion and see if I or any other player can disprove it
suggestbyothers(X) :- 
              write('Enter the suggested suspect: '),nl,read(_S),suspect(_S),
              write('Enter the suggested weapon: '),nl,read(_W),weapon(_W),
              write('Enter the suggested room: '),nl,read(_R),room(_R),!,
              next(X,Y),trydisprove(X,[_S,_W,_R],Y);
              write('Invalid input! Please check your spelling and try again.'),nl,suggestbyothers(X).

% Try to disprove one player's suggestion by me or by another player
trydisprove(X,S,X).
trydisprove(X,S,Y) :- X\=Y,not(db_myplayer(Y)),!,otherstrydisprove(X,S,Y).
trydisprove(X,S,Y) :- db_myplayer(Y),!,itrydisprove(X,S,Y).

% Check if other player passed or showed card to disprove a suggestion
otherstrydisprove(X,S,Y) :- db_playername(Y,Name),
                            write('Did '),write(Name),write(' "pass" or "show" a card?'),nl,
                            read(_A),passorshowothers(_A,X,S,Y),!;
                            write('Please type either "pass" or "show".'),nl,otherstrydisprove(X,S,Y).

% If one player passed disproving, store this passed player and the cards into db_passed;
% Then go to the next player to try disproving; 
% Otherwise, store the this player who showed card to the db_showed
passorshowothers(A,X,S,Y) :- A = pass,!,next(Y,Z), assert(db_passed(Y,S)), trydisprove(X,S,Z).
passorshowothers(A,X,S,Y) :- A = show,!,assert(db_showntoothers(Y,S)).

% Check if I can disprove other's suggestion with my cards
itrydisprove(X,[S,W,R],Y) :-   write('Can you disprove the suggestion? '),nl,
                               not(has_card(Y,S)),not(has_card(Y,W)),not(has_card(Y,R)),
                               write('No, you cannot disprove the suggestion, pass.'),!,nl,next(Y,Z),trydisprove(X,S,Z);
                               write('Yes, disprove the suggestion with one of card(s): '),write_cards(Y,[S,W,R]),nl.

write_cards(Y,[C]) :- not(has_card(Y,C)).
write_cards(Y,[C]) :- has_card(Y,C),write(C),write(' ').
write_cards(Y,[H|T]) :- has_card(Y,H),write(H),write(' '),write_cards(Y,T).
write_cards(Y,[H|T]) :- not(has_card(Y,H)),write_cards(Y,T).


% Prompts users to enter other player's accused suspect, weapon, and room. 
% It hands accusehelper to record truth/falsehood of accusation.
accusebyothers(X) :- write('Enter accused suspect'),nl,read(_S),suspect(_S),
             write('Enter accused weapon'),nl,read(_W),weapon(_W),
             write('Enter accused room'),nl,read(_R),room(_R),!,
             accusehelper(X,[_S,_W,_R]);
             write('Invalid input! Please check your spelling and retry.'),nl,accusebyothers(X).

% Prompts user to enter truth value of the accusation.
% It lets checkaccusation to makes changes accordingly.
accusehelper(X,S) :- write('Is accusation true or false ("t"/"f")?'),nl,
                read(_B),checkaccusation(_B,X,S);
                write('Invalid input, enter "t" or "f"!'),nl,accusehelper(X,S).
% If accusation is false, store false accusation into db_falseaccusation, failedplayer into db_failedplayer
% If accusation is true, terminate the game.
checkaccusation(B,X,S) :- B=f,!,assert(db_failedplayer(X)),assert(db_falseaccusation(S)).
checkaccusation(B,X,S) :- B=t,!,throw(gameover).

gameover :- throw(gameover).


/*
 * SETS AND MOVES
 */

% Make a new accuse or suggestion accordingly.
mymovehelper([accuse,S,W,R]) :- accusation([S,W,R]),!.
mymovehelper([suggest,S,W,R]) :- valid_murder_set([S,W,R]),not(has_one_of(P,[S,W,R])),
                                 good_suggestion([S,W,R]);valid_murder_set([S,W,R]),
                                 not(has_one_of(P,[S,W,R])).

% Make a suggestion combo that contains one of the focused choices
good_suggestion(S) :- narrow_down(C),member(C,S).

% Card C is unseen yet, a different card D is seen, both cards are within disproved combo that has at least 1 seencard
narrow_down(C) :- has_one_of(P,X),member(C,X),member(D,X),C\=D,valid(C),known_card(D).

% Break apart the list and check suspect, weapon, room, which makes up the murder combination
murder_set([S,W,R]) :- suspect(S),weapon(W),room(R).

% Check that the murder combination of suspect, weapon, room are legal and doesn't exist yet with helper functions
valid_murder_set([S,W,R]) :- murder_set([S,W,R]),not(db_falseaccusation([S,W,R])),valid(S),valid(W),valid(R).

% Check accusation for legal combination of elemts, the cards have not been seen yet, and they are likelycandidates
accusation([S,W,R]) :- valid_murder_set([S,W,R]),valid(S),murder_suspect(S),valid(W),murder_weapon(W),valid(R),murder_room(R).

:-dynamic db_mycard/1, db_passed/2, db_showntoothers/2, db_cardsshownme/2, db_falseaccusation/1.
db_falseaccusation(a,b,c).


/*
 * GENERAL PURPOSE PREDICATES
 */

% Cards that have never been seen or inferred yet
valid(X) :- card(X),not(known_card(X)).

known_card(X) :- card(X),player(Y),has_card(Y,X).

lacks_card(P,C) :- player(P),card(C),X \= P,has_card(X,C).
lacks_card(P,C) :- player(P),card(C),db_passed(P,S),member(C,S).

murder_suspect(X) :- suspect(X),invalid_suspects([A,B,C,D,E]),not(member(X,[A,B,C,D,E])),diff([A,B,C,D,E]),!.
murder_suspect(X) :- suspect(X), all_lack_card(X).
murder_weapon(X) :- weapon(X), all_lack_card(X),!.
murder_weapon(X) :- weapon(X),invalid_weapons([A,B,C,D,E]),not(member(X,[A,B,C,D,E])),diff([A,B,C,D,E]),!.
murder_room(X) :- room(X),invalid_rooms([A,B,C,D,E,F,G,H]),not(member(X,[A,B,C,D,E,F,G,H])),diff([A,B,C,D,E,F,G,H]),!.
murder_room(X) :- room(X), all_lack_card(X).

invalid_weapons([X]) :- weapon(X),known_card(X).
invalid_weapons([X|Y]) :- weapon(X),known_card(X),invalid_weapons(Y),not(member(X,Y)).

invalid_suspects([X]) :- suspect(X),known_card(X).
invalid_suspects([X|Y]) :- suspect(X),known_card(X),invalid_suspects(Y),not(member(X,Y)).

invalid_rooms([X]) :- room(X),known_card(X).
invalid_rooms([X|Y]) :- room(X),known_card(X),invalid_rooms(Y),not(member(X,Y)).

has_card(P,C) :- db_myplayer(P),db_mycard(C).
has_card(P,C) :- db_cardsshownme(P,C).
has_card(P,C) :- player(P),card(C),has_one_of(P,[C,W,R]),lacks_card(P,W),lacks_card(P,R).
has_card(P,C) :- player(P),card(C),has_one_of(P,[S,C,R]),lacks_card(P,S),lacks_card(P,R).
has_card(P,C) :- player(P),card(C),has_one_of(P,[S,W,C]),lacks_card(P,S),lacks_card(P,W).

has_one_of(P,X) :- player(P),murder_set(X),db_showntoothers(P,X).

all_lack_card(C) :- not(all_do_not_lack_card(C)).
all_do_not_lack_card(C) :- player(P),not(lacks_card(P,C)).

member(X,[X|T]).
member(X,[H|T]) :- member(X,T).

diff([H]).
diff([H|T]) :- not(member(H,T)),diff(T).


/*
 * THE CONTENTS OF THE DATABASE
 */

% Print all the known information
showall :- write('Here is the information I have collected so far: '),nl,nl,
           has_card(P,C),write(P),write(' has '),write(C).


:- write('*** PLEASE enter "start." to start The Clue Player Assistant ***'),nl,nl.

