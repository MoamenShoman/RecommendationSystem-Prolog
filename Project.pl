offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).


subSet([], []).
subSet([_|T], L):-
  subSet(T, L).
subSet([H|T], [H|L]):-
  subSet(T, L).

possibleSubset(L, R):-
  subSet(L, R1),
  permutation(R1, R).

possibleActivity([H|T], Acc, R):-
  H \== activity(_),
  possibleActivity(T, [H|Acc] , R).

possibleActivity([activity(X)|T], Acc, R):-
  possibleSubset(X, R1),
  reverse(Acc, RAcc),
  append(RAcc, [activity(R1)], L1),
  append(L1, T, R).

choosePreferences(Prefs , ChosenPreferences):-
   possibleSubset(Prefs , ChosenPreferences),
  \+member(activity(_), ChosenPreferences).

choosePreferences(Prefs , ChosenPreferences):-
   possibleSubset(Prefs , ChosenPreferences1),
  member(activity(_), ChosenPreferences1),
  possibleActivity(ChosenPreferences1, [], ChosenPreferences).

activitiesRate([],_, _, 0).

activitiesRate([H|T], C, Activities, S):-
  (member(H, Activities),
  customerPreferredActivity(C,H,R),
  activitiesRate(T, C, Activities, R1),
  S is R+R1) ;
  (\+ member(H, Activities),
   activitiesRate(T, C, Activities, S)).

preferenceSatisfaction(O, Customer, ChosenPrefs, S):-
  O=offer( _, Activities, _, _, _, _, _, _),

   ((member(activity(X),ChosenPrefs),
  activitiesRate(X,Customer, Activities, S1));
  (\+member(activity(_),ChosenPrefs),
   S1 is 0)),

  ((member(means(M),ChosenPrefs),
  offerMean(O,M),
  customerPreferredMean(Customer,M,S2));
  (\+member(means(_),ChosenPrefs),
  S2 is 0)),

  ((member(accommodation(A),ChosenPrefs),
  offerAccommodation(O,A),
  customerPreferredAccommodation(Customer,A,S3));
  (\+member(accommodation(_),ChosenPrefs),
  S3 is 0)),

  S is S1+S2+S3.

before(Y1-M1-D1 , Y2-M2-D2):-
  (Y1 < Y2);(Y1 = Y2 , M2 > M1);(Y1 = Y2 , M2 = M1 ,D2 >= D1).

overlapPeriod(period(Y11-M11-D11 ,Y12-M12-D12) , period(Y21-M21-D21 , Y22-M22-D22)):-
(before(Y11-M11-D11,Y21-M21-D21) , before(Y21-M21-D21 , Y12-M12-D12));
(before(Y11-M11-D11 , Y22-M22-D22) , before(Y22-M22-D22 , Y12-M12-D12)).


getOffer(ChosenPrefs, Offer):-
	  offerMean(Offer, M),
	  offerAccommodation(Offer, A),
	  Offer = offer(Destination, Activities, Cost, _, _, Period, _, _),

	  (member(dest(Destination), ChosenPrefs) ;
	  \+ member(dest(_), ChosenPrefs)),

	  ((member(activity(X), ChosenPrefs), matchActivities(X, Activities));
	  (\+member(activity(_), ChosenPrefs))),

	  ((member(budget(Y), ChosenPrefs), Y >= Cost) ;
	  (\+member(budget(_), ChosenPrefs))),

	  ((member(period(Z), ChosenPrefs), overlapPeriod(Z, Period)) ;
	  (\+member(period(_), ChosenPrefs))),

	  ((member(means(M), ChosenPrefs)) ;
	  (\+member(means(_), ChosenPrefs))) ,

	  ((member(accommodation(A), ChosenPrefs)) ;
	  (\+member(accommodation(_), ChosenPrefs))).

matchActivities([], _).
matchActivities([H|T], L):-
    member(H, L),
    matchActivities(T, L).


recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
  	choosePreferences(Prefs , ChosenPrefs),
  	getOffer(ChosenPrefs, O).


recommendOffer(Customers, PreferenceList , Offer , ChosenCustomers):-
	recommendOffer(Customers, PreferenceList , Offer,[], ChosenCustomers).

recommendOffer([C|CT], [P|PT] , Offer,Acc, ChosenCustomers):-
	recommendOfferForCustomer(P , ChosenPrefs , Offer),
	recommendOffer(CT,PT,Offer,[cusPrefs(C,ChosenPrefs) |Acc],ChosenCustomers).

recommendOffer([] , [] , Offer, Acc ,ChosenCustomers):-
  	Offer = offer(_,_,_,_,_,_,_,N),
	buildSatisfactionList(Acc ,SatisfactionList , Offer),
	sortSat(SatisfactionList,[],Res),
	pack(Res, Packed),
	permute(Packed, Permuted),
	my_flatten(Permuted, FinalRes),
	chooseN(FinalRes, N, ChosenCustomers).

buildSatisfactionList([cusPrefs(C,ChosenPrefs)|CT] , [sat(C,S)|Rest] , Offer):-
  	preferenceSatisfaction(Offer, C,ChosenPrefs, S),
	buildSatisfactionList(CT , Rest , Offer).

buildSatisfactionList([], [], _).


permute([H|T], [R1|R2]):-
	permutation(H, R1),
	permute(T, R2).
permute([], []).


sortSat([] , L1, L1).
sortSat( [sat(C,S)|T] , Acc , Res):-
 	insertInSorted(sat(C,S), Acc , NewAcc),
  	sortSat(T,NewAcc , Res).

insertInSorted(sat(C,S) , [] ,[sat(C,S)]).
insertInSorted(sat(C,S) , [sat(C1 ,S1) |T] , [sat(C,S),sat(C1,S1)|T]):-
	S >= S1.
insertInSorted(sat(C,S) , [sat(C1,S1)|T] , [sat(C1,S1)|T1]):-
  	S < S1,
  	insertInSorted(sat(C,S) , T , T1).

chooseN([], _ ,[]).
chooseN(_ ,0,[]).
chooseN([sat(C,_)|T], Num, [C|Res]):-
  	Num > 0 ,
  	Num1 is Num - 1,
  	chooseN(T, Num1 , Res).

pack([sat(C1, S1), sat(C2, S2)|T], Acc, [[sat(C1, S1)|Acc]|R]):-
	S1 \== S2,
	pack([sat(C2, S2)|T], [], R).

pack([sat(C1, S1), sat(C2, S2)|T], Acc, R):-
	S1 == S2,
	pack([sat(C2, S2)|T], [sat(C1, S1)|Acc], R).

pack([H], Acc, [[H|Acc]]).

pack(L, R):-
	pack(L, [], R).

my_flatten([], []).
my_flatten([H|T], X):-
	\+is_list(H),
	my_flatten(T, Y),
	append([H], Y, X).
my_flatten([H|T], X):-
	is_list(H),
	my_flatten(H, Y1),
	my_flatten(T, Y2),
	append(Y1, Y2, X).
