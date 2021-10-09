(* AUTOR - MATEUSZ ŁADYSZ *)
(* CODE REVIEWER - JAKUB PANASIUK *)

(* TYP *)

(* typ wartosc reprezentuje przedzial liczbowy:
- skladajacy sie z jednego spojnego przedzialu (Jeden - pojedyncze przedzialy)
- skladajacy sie z dwoch spojnych przedzialow (DWA - podwojne przedzialy) *)
type wartosc =
	| Jeden of float * float
	| Dwa of (float * float) * (float * float);;

(* DODATKOWE FUNKCJE *)

(* funkcja zwracajaca mniejsza z liczb a i b *)
(* minim(a, nan) = a *)
let minim (a : float) (b : float) =
	if classify_float a = FP_nan then b
	else if classify_float b = FP_nan then a
	else if a <= b then a
	else b;;

(* funkcja zwracajaca wieksza z liczb a i b *)
(* maxim(a, nan) = a *)
let maxim (a : float) (b : float) =
	if classify_float a = FP_nan then b
	else if classify_float b = FP_nan then a
	else if a <= b then b
	else a;;

(* funkcja laczaca dwa przedzialy a i b (suma dwoch przedzialow ) *)
let rec laczenie (a : wartosc) (b : wartosc) =
	match a, b with
	| Jeden (x, y), Jeden (x1, y1) ->
		if classify_float x = FP_nan then Jeden (x1, y1)
		else if classify_float x1 = FP_nan then Jeden (x, y)
		else if x <= x1 then
			if y >= x1 then Jeden (x, y1)
			else Dwa ((x, y), (x1, y1))
		else
			if y1 >= x then Jeden (x1, y)
			else Dwa ((x1, y1), (x, y))
	| Jeden (x, y), Dwa ((x1, y1), (z1, t1)) ->
		if classify_float x = FP_nan then Dwa ((x1, y1), (z1, t1))
		else if y1 >= x then laczenie (Jeden (x1, maxim y y1)) (Jeden (z1, t1))
		else Dwa ((x1, y1), (minim x z1, t1))
	| Dwa ((x, y), (z, t)), Jeden (x1, y1) ->
		if classify_float x1 = FP_nan then Dwa ((x, y), (z, t))
		else if y >= x1 then laczenie (Jeden (x, maxim y y1)) (Jeden (z, t))
		else Dwa ((x, y), (minim x1 z, t))
	| Dwa ((x, y), (z, t)), Dwa ((x1, y1), (z1, t1)) ->
		laczenie (Jeden (x, maxim y y1)) (Jeden (minim z z1, t));;

(* funkcja odwracajaca przedzial a (zwraca przedzial bedacy zbiorem odwrotnosci wszystkich liczb w a) *)
let odwroc (a : float * float) =
	let (x, y) = a
	in
		if (x = 0. && y = 0.) || classify_float x = FP_nan then Jeden (nan, nan)
		else if x = 0. then Jeden (1. /. y, infinity)
		else if y = 0. then Jeden (neg_infinity, 1. /. x)
		else if x < 0. && y > 0. then 
			laczenie (Jeden (neg_infinity, 1. /. x)) (Jeden (1. /. y, infinity))
		else Jeden (minim (1. /. x) (1. /. y), maxim (1. /. x) (1. /. y));;

(* KONSTRUKTORY *)

(* funkcja zwracajaca przedzial x +/- p% *)
(* zmienne a i b sa krancami powstajaego przedzialu *)
let wartosc_dokladnosc (x : float) (p : float) =
	let a = x -. ((p /. 100.) *. x) and b = x +. ((p /. 100.) *. x)
	in Jeden (minim a b, maxim a b);;

(* funkcja zwracajaca przedzial  (x, y) *)
let wartosc_od_do (x : float) (y : float) =
	Jeden (x, y);;

(* funkcja zwracajaca  przedzial (x, x) *)
let wartosc_dokladna (x : float) =
	Jeden (x, x);;

(* SELEKTORY *)

(* funkcja sprawdzajaca, czy y nalezy do przedzialu x *)
let in_wartosc (x : wartosc) (y : float) =
	match x with
	| Jeden (a, b) ->
		if classify_float a = FP_nan then false
    	else
    		if (y >= a && y <= b) then true
      		else false
	| Dwa ((a, b), (c, d)) -> 
		if (y >= a && y <= b) || (y >= c && y <= d) then true
    	else false;;

(* funkcja zwracajaca najmniejsza wartosc funkcji x *)
let min_wartosc (x : wartosc) =
	match x with
  | Jeden (a, _) ->
  	if classify_float a = FP_nan then nan
  	else a
  | Dwa ((a, _), (_, _)) -> a;;

(* funkcja zwracajaca najmniejsza wartosc funkcji x *)
  let max_wartosc (x : wartosc) =
  match x with
  | Jeden (_, a) ->
  	if classify_float a = FP_nan then nan
  	else a
  | Dwa ((_, _), (_, a)) -> a;;

(* funkcja zwracajaca srednia wartosc przedzialu x ((max_wartosc(x) + min_wartosc(x)) / 2) *)
let sr_wartosc (x : wartosc) =
  let a = min_wartosc x and b = max_wartosc x
  in
    if classify_float a = FP_nan || 
    	(a = neg_infinity && b = infinity) || 
    	classify_float b = FP_nan then nan
    else (a +. b) /. 2.;;

(* MODYFIKATORY *)

(* funkcja zwracajaca przedzial powstaly w wyniku dodania przedzialu b do przedzialu a -
do kazdej wartosci z przedzialu a dodajemy kazda wartosc z przedzialu b *)
let rec plus (a : wartosc) (b : wartosc) =
	match a, b with
	| Jeden (x, y), Jeden (x1, y1) ->
		if classify_float x = FP_nan || 
			classify_float x1 = FP_nan then Jeden (nan, nan)
		else Jeden (x +. x1, y +. y1)
	| Jeden (x, y), Dwa ((x1, y1), (z1, t1)) ->
		if classify_float x = FP_nan then Jeden (nan, nan)
		else laczenie (plus (Jeden (x, y)) (Jeden (x1, y1))) 
			(plus (Jeden (x, y)) (Jeden (z1, t1)))
	| Dwa ((x, y), (z, t)), Jeden (x1, y1) ->
		if classify_float x1 = FP_nan then Jeden (nan, nan)
		else laczenie (plus (Jeden (x, y)) (Jeden (x1, y1))) 
			(plus (Jeden (z, t)) (Jeden (x1, y1)))
	| Dwa ((x, y), (z, t)), Dwa ((x1, y1), (z1, t1)) ->
		laczenie (plus (Dwa ((x, y), (z, t))) (Jeden (x1, y1))) 
			(plus (Dwa ((x, y), (z, t))) (Jeden (z1, t1)));;

(* funkcja zwracajaca przedzial powstaly w wyniku odjecia przedzialu b od przedzialu a -
od kazdej wartosci z przedzialu a odejmujemy kazda wartosc z przedzialu b *)
(* dodawanie przedzialu a i przedzialu przeciwnego do b *)
let minus (a : wartosc) (b : wartosc) =
	match a, b with
	| Jeden (x, y), Jeden (x1, y1) ->
		plus (Jeden (x, y)) (Jeden (-.y1, -.x1))
	| Jeden (x, y), Dwa ((x1, y1), (z1, t1)) ->
		plus (Jeden (x, y)) (Dwa ((-.t1, -.z1), (-.y1, -.x1)))
	| Dwa ((x, y), (z, t)), Jeden (x1, y1) ->
		plus (Dwa ((x, y), (z, t))) (Jeden (-.y1, -.x1))
	| Dwa ((x, y), (z, t)), Dwa ((x1, y1), (z1, t1)) ->
		plus (Dwa ((x, y), (z, t))) (Dwa ((-.t1, -.z1), (-.y1, -.x1)));;

(* funkcja zwracajaca przedzial powstaly w wyniku wymnozenia przedzialu a i przedzialu b -
wymnazamy kazdy element z przedzialu a z kazdym elementem z przedzialu b *)
(* zmienne w1, w2, w3, w4 sa potencjalnymi krancami przedzialu powstalego z pomnozenia dwoch innych pojedynczych przedzialow *)
let rec razy (a : wartosc) (b : wartosc) =
	match a, b with
	| Jeden (x, y), Jeden (x1, y1) ->
		if classify_float x = FP_nan 
			|| classify_float x1 = FP_nan then Jeden (nan, nan)
		else if (x = 0. && y = 0.) || (x1 = 0. && y1 = 0.) then Jeden (0., 0.)
		else 
			let w1 = x *. x1 and w2 = x *. y1 and w3 = y *. x1 and w4 = y *. y1
			in Jeden (minim w1 (minim w2 (minim w3 w4)),maxim w1 (maxim w2 (maxim w3 w4)))
	| Jeden (x, y), Dwa ((x1, y1), (z1, t1)) ->
		laczenie (razy (Jeden (x, y)) (Jeden (x1, y1))) 
			(razy (Jeden (x, y)) (Jeden (z1, t1)))
	| Dwa ((x, y), (z, t)), Jeden (x1, y1) ->
		laczenie (razy (Jeden (x, y)) (Jeden (x1, y1))) 
			(razy (Jeden (z, t)) (Jeden (x1, y1)))
	| Dwa ((x, y), (z, t)), Dwa ((x1, y1), (z1, t1)) ->
		laczenie (razy (Dwa ((x, y), (z, t))) (Jeden (x1, y1))) 
			(razy (Dwa ((x, y), (z, t))) (Jeden (z1, t1)));;

(* funkcja zwracajaca przedzial powstaly w wyniku podzielenia przedzialu a przez przedzial b -
dzielimy kazda wartosc z przedzialu a przez kazda wartosc z przedzialu b *)
(* wymnazanie przedzialu a i odwrotnosci przedzialu b *)
let podzielic  (a : wartosc) (b : wartosc) =
	match a, b with
	| Jeden (x, y), Jeden (x1, y1) ->
		razy (Jeden (x, y)) (odwroc (x1, y1))
	| Jeden (x, y), Dwa ((x1, y1), (z1, t1)) ->
		razy (Jeden (x, y)) (laczenie (odwroc  (x1, y1)) (odwroc (z1, t1)))
	| Dwa ((x, y), (z, t)), Jeden (x1, y1) ->
		razy (Dwa ((x, y), (z, t))) (odwroc (x1, y1))
	| Dwa ((x, y), (z, t)), Dwa ((x1, y1), (z1, t1)) ->
		razy (Dwa ((x, y), (z, t))) (laczenie (odwroc  (x1, y1)) (odwroc (z1, t1)));;

(* TESTY *)
(* let a = sr_wartosc (podzielic (wartosc_dokladna (6.000000)) (wartosc_dokladna (2.600000))) ;;
assert (a = 2.3076923076923075);;
let a = min_wartosc (plus (plus (wartosc_od_do (-8.000000) (7.200000)) (wartosc_dokladnosc (0.000000) (2.000000))) (podzielic (wartosc_od_do (2.600000) (7.200000)) (wartosc_dokladnosc (0.000000) (0.600000)))) ;;
assert ((classify_float a) == FP_nan);;
let a = max_wartosc (plus (podzielic (wartosc_od_do (0.000000) (5.600000)) (podzielic (wartosc_dokladna (1.400000)) (wartosc_dokladnosc (-9.400000) (0.000000)))) (podzielic (podzielic (wartosc_dokladnosc (-9.200000) (3.600000)) (wartosc_dokladnosc (-6.800000) (0.000000))) (minus (razy (wartosc_dokladnosc (-4.400000) (8.600000)) (wartosc_od_do (-1.000000) (-0.600000))) (podzielic (wartosc_od_do (-6.800000) (-1.800000)) (wartosc_dokladna (0.000000)))))) ;;
assert ((classify_float a) == FP_nan);;
let a = max_wartosc (minus (wartosc_od_do (-6.200000) (0.000000)) (wartosc_dokladnosc (-8.800000) (8.600000))) ;;
assert (a = 9.5568);;
let a = sr_wartosc (podzielic (wartosc_od_do (-6.400000) (-4.400000)) (wartosc_dokladnosc (-5.400000) (6.200000))) ;;
assert (a = 1.01538461996061);;
let a = max_wartosc (minus (wartosc_od_do (5.600000) (6.200000)) (wartosc_dokladna (3.600000))) ;;
assert (a = 2.6);;
let a = sr_wartosc (razy (minus (wartosc_dokladna (7.400000)) (wartosc_od_do (0.000000) (0.600000))) (wartosc_dokladna (2.600000))) ;;
assert (a = 18.46);;
let a = min_wartosc (razy (wartosc_dokladnosc (1.400000) (0.000000)) (plus (wartosc_dokladnosc (-9.400000) (8.000000)) (wartosc_dokladna (9.000000)))) ;;
assert (a = -1.6128000000000013);;
let a = in_wartosc (podzielic (wartosc_dokladna (-4.800000)) (wartosc_dokladnosc (0.000000) (0.000000))) (-4.400000);;
assert (a = false);;
let a = in_wartosc (podzielic (podzielic (razy (wartosc_dokladnosc (-1.200000) (3.400000)) (podzielic (wartosc_od_do (0.000000) (4.400000)) (wartosc_od_do (-5.800000) (5.000000)))) (wartosc_od_do (1.400000) (3.800000))) (wartosc_dokladnosc (-8.600000) (9.800000))) (-4.200000);;
assert (a = true);;
let a = max_wartosc (razy (wartosc_dokladna (4.600000)) (wartosc_dokladnosc (-0.800000) (0.000000))) ;;
assert (a = -3.6799999999999997);;
let a = sr_wartosc (plus (wartosc_dokladnosc (-5.200000) (5.400000)) (podzielic (wartosc_dokladnosc (2.800000) (1.800000)) (wartosc_od_do (-4.600000) (1.400000)))) ;;
assert ((classify_float a) == FP_nan);;
let a = max_wartosc (razy (wartosc_od_do (-2.400000) (0.000000)) (razy (plus (wartosc_dokladna (6.000000)) (wartosc_dokladnosc (4.800000) (3.000000))) (wartosc_dokladna (0.000000)))) ;;
assert (a = 0.);;
let a = sr_wartosc (podzielic (wartosc_dokladnosc (5.600000) (0.000000)) (wartosc_dokladnosc (7.400000) (0.000000))) ;;
assert (a = 0.75675675675675658);;
let a = sr_wartosc (podzielic (wartosc_dokladna (0.000000)) (wartosc_dokladnosc (-8.000000) (8.800000))) ;;
assert (a = 0.);;
let a = in_wartosc (razy (wartosc_dokladna (1.600000)) (wartosc_od_do (-8.200000) (0.000000))) (4.800000);;
assert (a = false);;
let a = in_wartosc (razy (wartosc_od_do (1.800000) (3.600000)) (razy (wartosc_od_do (-4.800000) (9.800000)) (wartosc_od_do (-9.400000) (7.200000)))) (-7.800000);;
assert (a = true);;
let a = sr_wartosc (minus (minus (wartosc_dokladna (0.000000)) (wartosc_od_do (-5.200000) (1.200000))) (wartosc_dokladnosc (-1.600000) (0.200000))) ;;
assert (a = 3.6);;
let a = max_wartosc (podzielic (wartosc_dokladnosc (5.200000) (0.000000)) (minus (wartosc_dokladnosc (-3.800000) (9.600000)) (wartosc_dokladnosc (0.000000) (7.800000)))) ;;
assert (a = -1.2485593545908569);;
let a = podzielic (wartosc_dokladna 1.) (plus (podzielic (wartosc_dokladna 1.) (wartosc_od_do (-1.) (1.))) (wartosc_dokladna 2.));;
assert (in_wartosc a 0.99 = false);; *)