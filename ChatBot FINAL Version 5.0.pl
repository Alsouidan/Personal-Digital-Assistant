
%-------------------------------------------------------------
% res(-Sentence)
%-------------------------------------------------------------

res([FirstWord|RestOfSentence]) :-
  reSe([FirstWord|RestOfSentence]).

reSe([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord(Char,FirstWord,NextChar),
  readRestOfSentence(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res -------------------------
   readRestOfSentence(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence(_,Char,[NextWord|RestOfSentence]) :-
     readWord(Char,NextWord,NextChar),
     readRestOfSentence(NextWord,NextChar,RestOfSentence).

   readWord(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord(Char,Word,NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord(_,Word,NextChar) :-
     get0(TempChar),
     readWord(TempChar,Word,NextChar).

   restWord(Char,[NewChar|RestWord],NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar).
     restWord(Char,[],Char).

   singleCharWord(44).  /* , */
   singleCharWord(59).  /* ; */
   singleCharWord(58).  /* : */
   singleCharWord(63).  /* ? */
   singleCharWord(33).  /* ! */
   singleCharWord(46).  /* . */

   componentChar(Char,Char) :- Char>96,Char<123.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar(Char,Char) :- Char>47,Char<58.
   componentChar(39,39).  /* ' */
   componentChar(45,45).  /* - */
   componentChar(95,95).  /* _ */

   endOfSentenceWord('.').
   endOfSentenceWord('!').
   endOfSentenceWord('?').

%-------------------------------------------------------------
% res_pc(-Sentence)
%-------------------------------------------------------------

res_pc([FirstWord|RestOfSentence]) :-
  reSe_pc([FirstWord|RestOfSentence]).

reSe_pc([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord_pc(Char,FirstWord,NextChar),
  readRestOfSentence_pc(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res_pc -------------------------
   readRestOfSentence_pc(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence_pc(_,Char,[NextWord|RestOfSentence]) :-
     readWord_pc(Char,NextWord,NextChar),
     readRestOfSentence_pc(NextWord,NextChar,RestOfSentence).

   readWord_pc(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord_pc(Char,Word,NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord_pc(_,Word,NextChar) :-
     get0(TempChar),
     readWord_pc(TempChar,Word,NextChar).

   restWord_pc(Char,[NewChar|RestWord],NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar).
     restWord_pc(Char,[],Char).

   componentChar_pc(Char,Char) :- Char>96,Char<123.

   componentChar_pc(Char,Char) :- Char>64,Char<91.

   componentChar_pc(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar_pc(Char,Char) :- Char>47,Char<58.
   componentChar_pc(39,39).  /* ' */
   componentChar_pc(45,45).  /* - */
   componentChar_pc(95,95).  /* _ */

%-------------------------------------------------------------
% ws(+Sentence)
%-------------------------------------------------------------

ws([F|R]) :-
   write(F),
   wrs(R).

   %--- ancillaries to ws ------------------------
   wrs([F|R]) :-
     write(' '),
     write(F),
     wrs(R).
   wrs([]).

%-------------------------------------------------------------
% space/0
%-------------------------------------------------------------

space :- write(' ').

%-------------------------------------------------------------
% rs(-String)
%-------------------------------------------------------------

rs(S) :-
   get0(C),
   (
      C == -1,  S = [], !, fail;
      C == 10,  S = [], ! ;
      C == 32, !, rs(S);
      !, rs(C,S)
   ).

rs(C,[C|Cs]) :-
   get0(D),
   (
      D == -1,  Cs = [], !, fail;
      D == 10,  Cs = [], ! ;
      D == 32,  Cs = [], ! ;
      !, rs(D,Cs)
   ).


%-------------------------------------------------------------
% wrst(+String)
%-------------------------------------------------------------

wrst([]) :- !.
wrst([C|Cs]) :- put(C), wrst(Cs).



:-discontiguous(prop/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% VEGETABLES %%%%

prop(tomato,is,vegetable).
prop(onion,is,vegetable).
prop(bell_pepper,is,vegetable).
prop(chili_pepper,is,vegetable).
prop(carrot,is,vegetable).
prop(pea,is,vegetable).
prop(artichoke,is,vegetable).
prop(eggplant,is,vegetable).
prop(cucumber,is,vegetable).
prop(lettuce,is,vegetable).
prop(okra,is,vegetable).
prop(cauliflower,is,vegetable).
prop(cabbage,is,vegetable).
prop(broccoli,is,vegetable).
prop(mushroom,is,vegetable).
prop(potato,is,vegetable).
prop(zucchini,is,vegetable).
prop(broccoli,is,vegetable).
prop(spinach,is,vegetable).
prop(corn,is,vegetable).

%%%% FRUITS %%%%

prop(strawberry,is,fruit).
prop(blackberry,is,fruit).
prop(blueberry,is,fruit).
prop(banana,is,fruit).
prop(orange,is,fruit).
prop(grape,is,fruit).
prop(pineapple,is,fruit).
prop(apple,is,fruit).
prop(kiwi,is,fruit).
prop(peaches,is,fruit).
prop(guava,is,fruit).
prop(pear,is,fruit).
prop(mango,is,fruit).
prop(apricot,is,fruit).
prop(avocado,is,fruit).
prop(cherry,is,fruit).
prop(fig,is,fruit).
prop(coconut,is,fruit).
prop(lemon,is,fruit).
prop(watermelon,is,fruit).
prop(cantaloupe,is,fruit).

%%%% DIARY %%%%

prop(cheese,is,diary).
prop(milk,is,diary).
prop(yogurt,is,diary).

%%%% CARBS %%%%

prop(flour,is,carb).
prop(rice,is,carb).
prop(pasta,is,carb).
prop(chocolate,is,carb).

%%%% FATS %%%%

prop(oil,is,fat).
prop(butter,is,fat).

%%%% PROTEINS %%%%

prop(egg,is,protein).
prop(fish,is,protein).
prop(chicken,is,protein).
prop(meat,is,protein).
prop(shrimp,is,protein).
prop(minced_meat,is,protein).

%%%% DRESSING %%%%

prop(mayonnaise,is,dressing).
prop(vinegar,is,dressing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(chicken_caesar_salad,contain,chicken).
prop(chicken_caesar_salad,contain,oil).
prop(chicken_caesar_salad,contain,lettuce).
prop(chicken_caesar_salad,contain,cheese).
prop(chicken_caesar_salad,contain,mayonnaise).
prop(chicken_caesar_salad,contain,vinegar).
prop(chicken_caesar_salad,contain,bread).

prop(green_salad,contain,carrot).
prop(green_salad,contain,bell_pepper).
prop(green_salad,contain,lettuce).
prop(green_salad,contain,onion).
prop(green_salad,contain,tomato).
prop(green_salad,contain,cucumber).

prop(coleslaw_salad,contain,carrot).
prop(coleslaw_salad,contain,cabbage).
prop(coleslaw_salad,contain,mayonnaise).
prop(coleslaw_salad,contain,oil).

prop(pasta_salad,contain,bell_pepper).
prop(pasta_salad,contain,mayonnaise).
prop(pasta_salad,contain,pasta).
prop(pasta_salad,contain,corn).

prop(fruit_salad,contain,strawberry).
prop(fruit_salad,contain,banana).
prop(fruit_salad,contain,orange).
prop(fruit_salad,contain,apple).

prop(croissant,contain,butter).
prop(croissant,contain,flour).
prop(croissant,contain,milk).
prop(croissant,contain,oil).
prop(croissant,contain,egg).

prop(spanish_omelette,contain,egg).
prop(spanish_omelette,contain,oil).
prop(spanish_omelette,contain,potato).

prop(boiled_egg,contain,egg).

prop(grilled_chicken,contain,chicken).
prop(grilled_chicken,contain,lemon).
prop(grilled_chicken,contain,onion).

prop(fried_chicken,contain,chicken).
prop(fried_chicken,contain,oil).
prop(fried_chicken,contain,onion).
prop(fried_chicken,contain,flour).

prop(cake,contain,flour).
prop(cake,contain,butter).
prop(cake,contain,milk).
prop(cake,contain,egg).

prop(chocolate_cake,contain,cake).
prop(chocolate_cake,contain,chocolate).

prop(white_rice,contain,rice).
prop(white_rice,contain,butter).

prop(mexican_rice,contain,rice).
prop(mexican_rice,contain,oil).
prop(mexican_rice,contain,onion).
prop(mexican_rice,contain,tomato).

prop(ratatouille,contain,zucchini).
prop(ratatouille,contain,eggplant).
prop(ratatouille,contain,tomato).
prop(ratatouille,contain,bell_pepper).
prop(ratatouille,contain,onion).
prop(ratatouille,contain,lemon).
prop(ratatouille,contain,oil).
prop(ratatouille,contain,vinegar).

prop(lasagne,contain,pasta).
prop(lasagne,contain,milk).
prop(lasagne,contain,flour).
prop(lasagne,contain,butter).
prop(lasagne,contain,minced_meat).
prop(lasagne,contain,cheese).

prop(pasta_white_sauce,contain,pasta).
prop(pasta_white_sauce,contain,milk).
prop(pasta_white_sauce,contain,flour).
prop(pasta_white_sauce,contain,butter).

prop(pasta_red_sauce,contain,pasta).
prop(pasta_red_sauce,contain,tomato).
prop(pasta_red_sauce,contain,oil).

prop(pasta_alfredo,contain,pasta).
prop(pasta_alfredo,contain,milk).
prop(pasta_alfredo,contain,flour).
prop(pasta_alfredo,contain,butter).
prop(pasta_alfredo,contain,chicken).

prop(pasta_negresco,contain,pasta).
prop(pasta_negresco,contain,milk).
prop(pasta_negresco,contain,flour).
prop(pasta_negresco,contain,butter).
prop(pasta_negresco,contain,chicken).
prop(pasta_negresco,contain,cheese).

prop(shrimp_pasta,contain,pasta).
prop(shrimp_pasta,contain,shrimp).
prop(shrimp_pasta,contain,butter).
prop(shrimp_pasta,contain,milk).

prop(pizza,contain,tomato).
prop(pizza,contain,cheese).
prop(pizza,contain,flour).
prop(pizza,contain,oil).

prop(bread,contain,milk).
prop(bread,contain,flour).
prop(bread,contain,butter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(tomato,contain,11,cal).
prop(onion,contain,44,cal).
prop(cheese,contain,431,cal).
prop(egg,contain,78,cal).
prop(pasta,contain,131,cal).
prop(bell_pepper,contain,24,cal).
prop(chili_pepper,contain,18,cal).
prop(carrot,contain,25,cal).
prop(pea,contain,81,cal).
prop(artichoke,contain,120,cal).
prop(eggplant,contain,25,cal).
prop(cucumber,contain,32,cal).
prop(lettuce,contain,15,cal).
prop(okra,contain,33,cal).
prop(cauliflower,contain,25,cal).
prop(cabbage,contain,25,cal).
prop(broccoli,contain,31,cal).
prop(mushroom,contain,5,cal).
prop(potato,contain,163,cal).
prop(zucchini,contain,33,cal).
prop(spinach,contain,23,cal).
prop(corn,contain,86,cal).
prop(strawberry,contain,33,cal).
prop(blackberry,contain,43,cal).
prop(blueberry,contain,57,cal).
prop(banana,contain,89,cal).
prop(orange,contain,47,cal).
prop(grape,contain,62,cal).
prop(pineapple,contain,42,cal).
prop(apple,contain,92,cal).
prop(kiwi,contain,42,cal).
prop(peaches,contain,59,cal).
prop(guava,contain,38,cal).
prop(pear,contain,85,cal).
prop(mango,contain,99,cal).
prop(apricot,contain,48,cal).
prop(avocado,contain,160,cal).
prop(cherry,contain,50,cal).
prop(fig,contain,107,cal).
prop(coconut,contain,283,cal).
prop(lemon,contain,24,cal).
prop(watermelon,contain,30,cal).
prop(cantaloupe,contain,34,cal).
prop(milk,contain,124,cal).
prop(yogurt,contain,218,cal).
prop(flour,contain,364,cal).
prop(rice,contain,150,cal).
prop(oil,contain,240,cal).
prop(butter,contain,204,cal).
prop(fish,contain,305,cal).
prop(chicken,contain,335,cal).
prop(meat,contain,250,cal).
prop(shrimp,contain,85,cal).
prop(minced_meat,contain,332,cal).
prop(mayonnaise,contain,188,cal).
prop(vinegar,contain,3,cal).
prop(chocolate,contain,137,cal).
%prop(,contain,,cal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(cheese,not,lunch).
prop(yogurt,not,lunch).
prop(boiled_egg,not,lunch).
prop(boiled_egg,not,dinner).
prop(spanish_omelette,not,lunch).
prop(spanish_omelette,not,dinner).
prop(croissant,not,lunch).
prop(chicken_caesar_salad,not,breakfast).
prop(chicken_caesar_salad,not,dinner).
prop(pizza,not,breakfast).
prop(shrimp_pasta,not,breakfast).
prop(shrimp_pasta,not,dinner).
prop(pasta_negresco,not,breakfast).
prop(pasta_negresco,not,dinner).
prop(pasta_alfredo,not,breakfast).
prop(pasta_alfredo,not,dinner).
prop(pasta_red_sauce,not,breakfast).
prop(pasta_red_sauce,not,dinner).
prop(pasta_white_sauce,not,breakfast).
prop(pasta_white_sauce,not,dinner).
prop(fried_chicken,not,breakfast).
prop(fried_chicken,not,dinner).
prop(grilled_chicken,not,breakfast).
prop(grilled_chicken,not,dinner).
prop(lasagne,not,breakfast).
prop(lasagne,not,dinner).
prop(ratatouille,not,breakfast).
prop(ratatouille,not,dinner).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous readInputTillQuit/3.
readHelper([],[],R,R).
readHelper([A|T],[_|T1],ACC,R):-[A]\=[i,ate,_,for,_,.],[A]\=[can,i,have,_,for,_,?],
                                 readHelper(T,T1,ACC,R).
readHelper([[i,ate,A,for,B,.]|T1],[[ok,.]|T2],ACC,R):-
                       readHelper(T1,T2,[[you,had,A,for,B,.]|ACC],R).
                       
readHelper([[can,i,have,A,for,B,?]|T1],[[you,can,have,A,for,B,.]|T2],ACC,R):-
                       readHelper(T1,T2,[[you,had,A,for,B,.]|ACC],R).
                       
readHelper([[i,ate,_,for,_,.]|T1],[[you,told,me,that,before,.]|T2],ACC,R):-
                       readHelper(T1,T2,ACC,R).
readHelper([[can,i,have,A,for,B,?]|T1],[[R2]|T2],ACC,R):-
                        R2\=[you,can,have,A,for,B,.],
                       readHelper(T1,T2,ACC,R).

printer([]).
printer([H|T]):-
                write("> "),
                ws(H),
                nl,
                printer(T).
%%%%%%%%%%%%%%%quit summary%%%%%%%%%%%%%%%we can arrange write statements better
readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   member([you,had,_,for,breakfast,.],R) ,
                                   member([you,had,_,for,lunch,.],R) ,
                                   member([you,had,_,for,dinner,.],R) ,
                                   printer(R),
                                   write("> Bye.").

readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   \+member([you,had,_,for,breakfast,.],R) ,
                                   member([you,had,_,for,lunch,.],R) ,
                                   member([you,had,_,for,dinner,.],R) ,
                                   write("> you had - for breakfast"),
                                   nl,
                                   printer(R),
                                   write("> Bye.").

readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   member([you,had,_,for,breakfast,.],R) ,
                                   \+member([you,had,_,for,lunch,.],R) ,
                                   member([you,had,_,for,dinner,.],R) ,
                                   write("> you had - for lunch"),
                                   nl,
                                   printer(R),
                                   write("> Bye.").
                                   
readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   member([you,had,_,for,breakfast,.],R) ,
                                   member([you,had,_,for,lunch,.],R) ,
                                   \+member([you,had,_,for,dinner,.],R) ,
                                   write("> you had - for dinner"),
                                   nl,
                                   printer(R),
                                   write("> Bye.").
                                   
readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   \+member([you,had,_,for,breakfast,.],R) ,
                                   \+member([you,had,_,for,lunch,.],R) ,
                                   member([you,had,_,for,dinner,.],R) ,
                                   write("> you had - for breakfast"),
                                   nl,
                                   write("> you had - for lunch"),
                                   nl,
                                   printer(R),
                                   write("> Bye.").
                                   
readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   \+member([you,had,_,for,breakfast,.],R) ,
                                   member([you,had,_,for,lunch,.],R) ,
                                   \+member([you,had,_,for,dinner,.],R) ,
                                   write("> you had - for breakfast"),
                                   nl,
                                   write("> you had - for dinner"),
                                   nl,
                                   printer(R),
                                   write("> Bye.").
                                   
readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   member([you,had,_,for,breakfast,.],R) ,
                                   \+member([you,had,_,for,lunch,.],R) ,
                                   \+member([you,had,_,for,dinner,.],R) ,
                                   write("> you had - for lunch"),
                                   nl,
                                   write("> you had - for dinner"),
                                   nl,
                                   printer(R),
                                   write("> Bye.").
                                   
readInputTillQuit([quit,.],PQ,PR):-
                                   readHelper(PQ,PR,[],R),
                                   \+member([you,had,_,for,breakfast,.],R) ,
                                   \+member([you,had,_,for,lunch,.],R) ,
                                   \+member([you,had,_,for,dinner,.],R) ,
                                   write("> you had - for breakfast"),
                                   nl,
                                   write("> you had - for lunch"),
                                   nl,
                                   write("> you had - for dinner"),
                                   nl,
                                   write("> Bye.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%end quit summary%%%%%%%%%%%%%%%%%%%%
readInputTillQuit():-  write("> Welcome to your personal assistant"),
                       nl,
                       write("> "),
                       res(X),
                       readInputTillQuit(X,[],[]).

readInputTillQuit(Y,PQ,PR):-
                       Y\=[quit,.],
                       \+isValid(Y),
                       write("> I cannot understand you."),
                       nl,
                       write("> "),
                       res(X),  %Postopone
                       readInputTillQuit(X,[Y|PQ],[[i,cannot,understand,you,.]|PR]).

readInputTillQuit(Y,PQ,PR):-
                       Y\=[quit,.],
                       isValid(Y),
                       response(Y,PQ,PR,R),
                       R=[i,told,you,that,before,.],
                       write("> "),
                       ws(R),
                       nl,
                       write("> "),
                       res(X),  %Postopone
                       readInputTillQuit(X,PQ,PR).

readInputTillQuit(Y,PQ,PR):-
                       Y\=[quit,.],
                       isValid(Y),
                       response(Y,PQ,PR,R),
                       R\=[i,told,you,that,before,.],
                       write("> "),
                       ws(R),
                       nl,
                       write("> "),
                       res(X),  %Postopone
                       readInputTillQuit(X,[Y|PQ],[R|PR]).
                       

                       
isValid([how,many,calories,does,_,contain,?]).
isValid([what,does,_,contain,?]).
isValid([can,i,have,_,for,_,?]).
isValid([what,is,_,?]).
isValid([how,many,calories,do,i,have,left,?]).
isValid([what,kind,of,_,does,_,contain,?]).
isValid([is,_,a,_,in,_,?]).
isValid([what,can,i,have,for,_,that,contains,_,?]).
isValid([i,ate,_,for,_,.]).
isValid([i,do,not,eat,_,.]).
isValid([_,is,_,.]).
isValid([quit,.]).
filterPropHelper(Relation,[A,B]):-
prop(A,Relation,B).
filterProp(Relation,R):-
                        setof(L,filterPropHelper(Relation,L),R).
matchFirst(_,[],[]).
matchFirst(T1,[[T1,B]|T],[B-1|C]):-     %%as filterProp returns a list of lists
matchFirst(T1,T,C).                     %matchFirst and matchScond takes a list where each element is a list containing one pari
matchFirst(T1,[[A,B]|T],[B-0|C]):-
T1\=A,matchFirst(T1,T,C).
matchSecond(_,[],[]).
matchSecond(T1,[[B,T1]|T],[B-1|C]):-
matchSecond(T1,T,C).
matchSecond(T1,[[B,N]|T],[B-0|C]):-
T1\=N,matchSecond(T1,T,C).
mergeMatchList(ML1,ML2,R):-
                           helpMerge(ML1,ML2,[],R).
addOcc(A,B,R):-
               addOcc1(A,B,[],R).
addOcc1(E-O,[E-O1|T],L,R):-
                        O2 is O+O1,
                        append(L,[E-O2|T],R).
addOcc1(E-O,[],L,R):-
                    R=[E-O|L].
addOcc1(E-O,[B-O1|T],L,R):-
                          B\=E,
                          addOcc1(E-O,T,[B-O1|L],R).
helpMerge([],[],R,R).
helpMerge([E-O|T1],[],T,R):-
                            addOcc(E-O,T,T2),
                            helpMerge(T1,[],T2,R).
helpMerge([],[E-O|T1],T,R):-
                            addOcc(E-O,T,T2),
                            helpMerge([],T1,T2,R).
helpMerge([E-O|T1],[],T,R):-
                            addOcc(E-O,T,T2),
                            helpMerge(T1,[],T2,R).
helpMerge([E-O1|T1],[E-O2|T2],T,R):-
                                          N is O1 +O2,
                                          addOcc(E-N,T,R1),
                                          helpMerge(T1,T2,R1,R).

helpMerge([E-O1|T1],[F-O2|T2],T,R):-
                                    E\=F,
                                    addOcc(E-O1,T,L3),
                                    addOcc(F-O2,L3,L4),
                                    helpMerge(T1,T2,L4,R).
bestMatchesMin(E,N,R):-
                             bestMinHelper(E,N,[],R).
bestMinHelper([],_,R,R).
bestMinHelper([E-O|T],N,L,R):-
                              O>=N,
                              bestMinHelper(T,N,[E|L],R).
bestMinHelper([_-O|T],N,L,R):-
                              O<N,
                              bestMinHelper(T,N,L,R).

bestMatches(A,R):-
                  bestHelper(A,0,[],R).
bestHelper([],_,R,R).
bestHelper([E-O|T],O,L,R):-
                     append(L,E,L1),
                     bestHelper(T,O,L1,R).
bestHelper([_-O|T],Max,L,R):-
                     Max>O,
                     bestHelper(T,Max,L,R).
bestHelper([E-O|T],Max,_,R):-
                     Max<O,
                     bestHelper(T,O,[E],R).
                     
bestMatchWOcc(A,R):- bestHelper2(A,0,[],R).
bestHelper2([],_,R,R).
bestHelper2([E-O|T],O,L,R):-
 bestHelper2(T,O,[E-O|L],R).
bestHelper2([_-O|T],Max,L,R):-
                     Max>O,
                     bestHelper2(T,Max,L,R).
bestHelper2([E-O|T],Max,_,R):-
                     Max<O,
                     bestHelper2(T,O,[E-O],R).

calcCalList([],0).
calcCalList([A|T],C):-
%                       prop(A,contain,C1,cal),
                        foodCal(A,C1),
                      calcCalList(T,C2),
                      C is C1+C2.
getIngredients(A,B):-
                    bagof(N,prop(A,contain,N),B).
foodCal(FL,C):-
        \+prop(FL,contain,C,cal),
       getIngredients(FL,B),
       calcCalList(B,C).
foodCal(FL,C):-
       prop(FL,contain,C,cal).
foodCalList([A|T],C):-
foodCal(A,C1),foodCalList(T,C2), C is C2+C1.
foodCalList([],0).
totalCal(1800).    %%%%%%%%%%%%%total cal%%%%%%%%%%%%%%%%%%%


calcCalories2(B,C,D):-
  calcHelper(B,C,D1),
  totalCal(X),
  D is X-D1.

calcCalories(A,B,C,D):-
foodCal(A,D2),
calcHelper(B,C,D1),
totalCal(X),
D is X-D1-D2.

calcHelper([],[],0).
calcHelper([Q|T],[_|T1],C):-
Q\=[i,ate,_,for,_,.],Q\=[can,i,have,_,for,_,?],calcHelper(T,T1,C).

calcHelper([[i,ate,F,for,_,.]|T],[[ok,.]|T1],C):-
                                                 foodCal(F,C1),
                                                 calcHelper(T,T1,C2),
                                                 C is C1 + C2.

calcHelper([[i,ate,_,for,_,.]|T],[R|T1],C):-
  R\=[ok,.],
  calcHelper(T,T1,C).

calcHelper([[can,i,have,A,for,_,?]|T],[[you,can,have,A,for,_,.]|T1],C):-
 foodCal(A,C1),
 calcHelper(T,T1,C2),
 C is C1+C2.

calcHelper([[can,i,have,A,for,_,?]|T],[Q|T1],C):-
  Q\=[can,i,have,A,for,_,?],
  calcHelper(T,T1,C).
                                                                     
getDiffAnswer(Q,[Q|_],PR,[CR|_],CR):-
\+member([CR],PR).
getDiffAnswer(Q,[Q|_],PR,[CR|T],R):-
member([CR],PR),
getDiffAnswer(Q,[Q|_],PR,T,R).


getDiffAnswer(Q,[Q|T],[[CR]|T1],[CR|T3],R):-
                                         getDiffAnswer(Q,T,T1,T3,R).

getDiffAnswer(Q,[Q1|T],[_|T1],T3,R):-
                    Q\=Q1,
                    getDiffAnswer(Q,T,T1,T3,R).
getDiffAnswer(_,[],[],[R|_],R).
listOrderDesc(L,R):-
                        listHelper(L,[],R).
getMax(A,R):-
                  getMaxHelper(A,-1,[],R).
getMaxHelper([],_,R,R).
getMaxHelper([E-O|T],O,L,R):-
                     getMaxHelper(T,O,[E-O|L],R).
getMaxHelper([_-O|T],Max,L,R):-
                     Max>O,
                     getMaxHelper(T,Max,L,R).
getMaxHelper([E-O|T],Max,_,R):-
                     Max<O,
                     getMaxHelper(T,O,[E-O],R).
remove(B,[],B).
remove(B,[A|T],R):-
                 delete(B,A,R1),
                 remove(R1,T,R).
listHelper([],R,R).

listHelper(E,A,R):-
                         E\=[],
                         getMax(E,F),
                         append(A,F,R1),
                         remove(E,F,R2),
                         listHelper(R2,R1,R).

foodFromHistory(A,R):-
                      foodHelper(A,[],R).
foodHelper([],R,R).
foodHelper([[i,ate,A,for,_,.]|T],B,R):-
                                     append(B,[A],R1),
                                     foodHelper(T,R1,R).
foodHelper([[you,can,have,A,for,_,.]|T],B,R):-
                                     append(B,[A],R1),
                                     foodHelper(T,R1,R).
foodHelper([A|T],B,R):-
                       A\=[you,can,have,_,for,_,.],
                       A\=[i,ate,_,for,_,.],
                       foodHelper(T,B,R).

getUnlikedIngredients(A,R):-
                      unlikeHelper(A,[],R).
unlikeHelper([],R,R).
unlikeHelper([[i,do,not,eat,A,.]|T],B,R):-
                                     append(B,[A],R1),
                                     unlikeHelper(T,R1,R).

unlikeHelper([A|T],B,R):-
                       A\=[i,do,not,eat,_,.],
                       unlikeHelper(T,B,R).
                       
                       
%%%%%%%%%%%responseO%%%%%%%%%%%
oHelper2([],_).
oHelper2([I|T],UI):-
                    \+member(I,UI),
                    oHelper2(T,UI).
responseO(Q,PQ,PR,L):-Q=[what,can,i,have,for,_,that,contains,B,?],   %%with a max score of 3 not 4
                     getUnlikedIngredients(PQ,R),
                     bagof(X,prop(X,contain,B),R1),
                     oHelper(Q,R1,PQ,PR,R,[],L1),
                     listOrderDesc(L1,L).
                     
 oHelper(_,[],_,_,_,R,R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C>=0,
 getIngredients(A,LI),
 oHelper2(LI,UI),
 \+prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-3|ACC],R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C<0,
  getIngredients(A,LI),
 oHelper2(LI,UI),
 \+prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-2|ACC],R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C>=0,
  getIngredients(A,LI),
 \+oHelper2(LI,UI),
 \+prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-2|ACC],R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C>=0,
  getIngredients(A,LI),
 oHelper2(LI,UI),
 prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-2|ACC],R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C<0,
  getIngredients(A,LI),
 \+oHelper2(LI,UI),
 \+prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-1|ACC],R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C>=0,
 getIngredients(A,LI),
 \+oHelper2(LI,UI),
 prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-1|ACC],R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C<0,
 getIngredients(A,LI),
 oHelper2(LI,UI),
 prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-1|ACC],R).
 oHelper([what,can,i,have,for,F,that,contains,_,?],[A|T],PQ,PR,UI,ACC,R):-
 calcCalories(A,PQ,PR,C),
 C<0,
 getIngredients(A,LI),
 \+oHelper2(LI,UI),
 prop(A,not,F),
 oHelper([what,can,i,have,for,F,that,contains,_,?],T,PQ,PR,UI,[A-0|ACC],R).
 


                     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%start response%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
response([what,kind,of,A,does,B,contain,?],_,_,[i,do,not,know,.]) :-
  ((\+prop(_,is,A));
  (\+prop(B,contain,_))).




response(Q,PQ,PR,[R]) :-
  Q = [what,kind,of,FC,does,F,contain,?],
  prop(F,contain,R),
  prop(R,is,FC),
  filterProp(contain,L1),
  filterProp(is,L2),
  matchFirst(F,L1,R1),
  matchSecond(FC,L2,R2),
  bestMatchWOcc(R1,B1),
  bestMatchWOcc(R2,B2),
  mergeMatchList(B1,B2,L3),
  bestMatchesMin(L3,2,CR),
  length(CR,N),
  N >= 1,
  getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,_,_,[nothing,from,what,i,know,.]) :-
  Q = [what,kind,of,FC,does,F,contain,?],
  prop(F,contain,_),
  prop(_,is,FC),
  filterProp(contain,L1),
  filterProp(is,L2),
  matchFirst(F,L1,R1),
  matchSecond(FC,L2,R2),
  bestMatchWOcc(R1,B1),
  bestMatchWOcc(R2,B2),
  mergeMatchList(B1,B2,L3),
  bestMatchesMin(L3,2,CR),
  length(CR,0).




response(Q,PQ,PR,[i,told,you,that,before,.]) :-
  Q = [what,kind,of,FC,does,F,contain,?],
  prop(F,contain,R),
  prop(R,is,FC),
  filterProp(contain,L1),
  filterProp(is,L2),
  matchFirst(F,L1,R1),
  matchSecond(FC,L2,R2),
  bestMatchWOcc(R1,B1),
  bestMatchWOcc(R2,B2),
  mergeMatchList(B1,B2,L3),
  bestMatchesMin(L3,2,CR),
  length(CR,N),
  N >= 1,
  \+getDiffAnswer(Q,PQ,PR,CR,_).
%%%%%%%%%%%%%%%%%End of what kind of A does B contain%%%%%%%%%%%
response(Q,PQ,_,[R,calories,.]):-
Q=[how,many,calories,does,A,contain,?],
\+member(Q,PQ),
foodCal(A,R).

response(Q,PQ,_,[i,told,you,that,before,.]):-
Q=[how,many,calories,does,_,contain,?],
member(Q,PQ).
response(Q,_,_,[i,do,not,know,.]):-
Q=[how,many,calories,does,A,contain,?] ,
\+foodCal(A,_) .

response(Q,PQ,PR,[R]):-
Q=[what,does,A,contain,?],
getIngredients(A,R1),
getDiffAnswer(Q,PQ,PR,R1,R).

response(Q,PQ,PR,[i,told,you,that,before,.]):-
Q=[what,does,A,contain,?],
getIngredients(A,R1),
\+getDiffAnswer(Q,PQ,PR,R1,_).
response(Q,_,_,[i,do,not,know,.]):-
Q=[what,does,A,contain,?],
\+getIngredients(A,_).

response(Q,PQ,PR,[you,can,have,A,for,B,.]):-
Q=[can,i,have,A,for,B,?],
\+prop(A,not,B),
calcCalories(A,PQ,PR,C),
C>=0,
\+member(Q,PQ).

response(Q,PQ,PR,[no,.]):-
Q=[can,i,have,A,for,B,?],
\+prop(A,not,B),
calcCalories(A,PQ,PR,C),
C<0.

response(Q,PQ,_,[A,is,not,suitable,for,B,.]):-
Q=[can,i,have,A,for,B,?],
prop(A,not,B),
\+member(Q,PQ).


response([can,i,have,A,for,B,?],_,_,[i,do,not,know,.]):-
\+prop(A,contain,_);\+prop(_,not,B).

response(Q,PQ,_,[R]):-
Q=[what,is,A,?],
prop(A,is,R),
\+member(Q,PQ).

response(Q,PQ,_,[i,told,you,that,before,.]):-
Q=[what,is,A,?],
prop(A,is,_),
member(Q,PQ).

response(Q,_,_,[i,do,not,know,.]):-
Q=[what,is,A,?],
\+prop(A,is,_).

response([how,many,calories,do,i,have,left,?],PQ,PR,R):-
R=[you,have,R1,calories,left,.] ,
calcCalories2(PQ,PR,R1).



response(Q,PQ,_,[yes,.]):-
Q=[is,A,a,B,in,C,?],
prop(C,contain,A),
prop(A,is,B),
\+member(Q,PQ).
response(Q,PQ,_,[no,.]):-
Q=[is,A,a,B,in,C,?],
((\+prop(C,contain,A));(\+prop(A,is,B))),
\+member(Q,PQ).
response(Q,PQ,_,[i,told,you,that,before,.]):-
Q=[is,_,a,_,in,_,?],
member(Q,PQ).
response([what,can,i,have,for,A,that,contains,B,?],_,_,[i,do,not,know,.]) :-
  ((\+prop(_,contain,B));
  (\+prop(_,not,A))).

response(Q,PQ,PR,[R]):-
Q=[what,can,i,have,for,A,that,contains,B,?],
prop(X,contain,B),\+prop(X,not,A),
responseO(Q,PQ,PR,CR1),
bestMatchesMin(CR1,3,CR2),
reverse(CR2,CR),
length(CR,N),
N>0,
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,[nothing,from,what,i,know,.]):-
Q=[what,can,i,have,for,_,that,contains,_,?],
responseO(Q,PQ,PR,CR1),
bestMatchesMin(CR1,3,CR2),
reverse(CR2,CR),
length(CR,0).

response(Q,PQ,PR,[i,told,you,that,before,.]):-
Q=[what,can,i,have,for,A,that,contains,B,?],
prop(X,contain,B),\+prop(X,not,A),
responseO(Q,PQ,PR,CR1),
bestMatchesMin(CR1,3,CR2),
reverse(CR2,CR),
length(CR,N),
N>0,
\+getDiffAnswer(Q,PQ,PR,CR,_).

response(Q,PQ,_,[ok,.]):-
Q=[i,ate,_,for,_,.],
\+member(Q,PQ).


response(Q,PQ,_,[you,told,me,that,before,.]):-
Q=[i,ate,_,for,_,.],
member(Q,PQ).

response(Q,PQ,_,[ok,.]):-
Q=[i,do,not,eat,_,.],
%prop(_,contain,A),
\+member(Q,PQ).

response(Q,PQ,_,[you,told,me,that,before,.]):-
Q=[i,do,not,eat,_,.],
member(Q,PQ).

%%%%%%%%%%When inputting a question end with a question mark%%%%%%%%
%%%%%%%%%when inputting a statement end with a full stop%%%%%%%%%%%%
%All predicates are made so that any sentence's list should end with a '.' or
%a'?' ,for example response([what,is,fig,?],[],[],R). must have the question
%mark at the end of the list%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





                    




