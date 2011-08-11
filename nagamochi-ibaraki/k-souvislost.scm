; autor:   Jakl Tomas (2009)
; email:   jacke.lee@volny.cz
; licence: GPLv2 a vyšší (viz http://www.gnu.org/licenses/gpl-2.0.html)

; Popis algoritmu (Nagamochi, Ibaraki):
; Algoritmus využívá takzvaného legálního uspořádání vrcholů.
;
; Mějme neorientovaný graf G=(V,E) s nezáporným ohodnocením hran.
; Značení:
;    d(M, v) je kapacita hran vedoucích z množiny vrcholů M (podmnožina V) do v
;    d(v) je kapacita hran vedoucích z v
;    r(u, v) je kapacita minimálního u,v-řezu
;
; Legální uspořádání vrcholů je takové lineární uspořádání vrcholů v_1, ... v_n,
; že platí:
;    d({v_1 ... v_i-1}, v_i) >= d({v_1 ... v_i-1}, v_j)
;    pro všechny 1 <= i < j <= n.
;
; Potom platí, že je-li v_1 ... v_n legální uspořádání vrcholů na G, pak
;    r(v_n-1, v_n) = d(v_n)                                     [důkaz viz: [1]]
;
; Potom buď globálně minimální řez odděluje vrcholy v_n-1 a v_n, nebo neodděluje
; -> najdeme jej rekurzivně v grafu, ve kterém jsou v_n-1 a v_n zkontrahované.
;
; Rozbor časové složitosti (n je # vrcholu a m je # hran):
;    předpřipravení grafu - O(n+m)
;
;    složitost prioritní fronty:
;       insert v O(n)
;       delete v O(1)
;       increase v O(n)
;
;    legalní uspořádání:
;       pro každý vrchol
;          přidáme do fronty v O(n)
;          smažeme maximum z fronty v O(1)
;          opravíme priority vrcholů podle hran do odebraného v O(mn)
;       = dohromady (m*n^2)
;
;    kontrahování posledních dvou vrcholu legálního uspořádání:
;       spojení hran vedoucích z těchto vrcholů v O(m)
;       přesměrování a sečtení hran ve zbytku grafu v O(mn)
;
;    celkem tedy samotný algoritmus:
;       předpřipravení grafu v O(m+n)
;       řádově n-krát
;          sestavíme legální uspořádání
;          zjistíme aktuální řez mezi posledními prvky leg. usp. v O(1)
;          zkontrahujeme graf
;       = celkem O(m*n^3)
;
; Časová složitost v [1] je O(n * (m + n log n)) je zhoršená kvůli použité
; prioritní frontě namísto fibonačiho haldy, která by přidání a úpravu prvku ve
; struktuře měla v konstantním čase a hledání maxima v logaritmickém.
;
; Časová složitost asi neúplně souhlasí s literaturou, provedl jsem asi
; myšlenkové zkratky, které nemám potřebu ověřovat. Radši se podívejte do [1].
;
; převzato z:
;    [1] Marin Mareš, Krajinou grafových algoritmů, ITI 2007


; Fronta:
;   serazeny seznam prvku podle priority (sestupne)

; Prvek:
;  ((cislo . seznam-sousedu) . priorita)
(define (prvek cislo priorita seznam-sousedu)
	(cons (cons cislo seznam-sousedu) priorita))


; primy pristup k informacim prvku fronty
(define (p-cislo p)    (car (car p)))
(define (p-priorita p) (cdr p))
(define (p-sousedi p)  (cdr (car p)))


; Prida prvek do fronty.
;
; predpoklada se, ze prvek p je neprazdny
(define (fronta-add f p)
	(if (null? f)
		(cons p '())
		(let ((hf (car f)))
			(if (>= (p-priorita p) (p-priorita hf))
				(cons p f)
				(cons hf (fronta-add (cdr f) p))))))


; Zmeni prioritu prvku ve fronte.
;
; POZOR p - zde dvojice (cislo-prvku . zmena-priority)
; uvazuji se jen kladne zmeny priority
(define (fronta-inc f p)
	(if (null? f)
		'()

		(let ((hf (car f)))
			(if (= (p-cislo hf) (car p))
				(cons
					(prvek
						(car p)
						(+ (cdr p) (p-priorita hf))
						(p-sousedi (car f)))
					(cdr f))
				(let ((f-inc (fronta-inc (cdr f) p)))
					(if (or (null? f-inc) (> (p-priorita hf) (p-priorita (car f-inc))))
						(cons hf f-inc)
						(cons (car f-inc) (cons hf (cdr f-inc)))))))))


; Smaze maximum z fronty.
;
; vrati dvojici (zbytek_fronty . (maximum . priorita_maxima))
; f = nil | prvek,f
(define (del-max f)
	(if (null? f)
		(cons '() '())
		(cons (cdr f) (car f))))


; vraci "stupen" vrcholu odpovidajici souctu kapacit vsech
; hran z nej vedoucich
(define (deg v)
	(if (null? v)
		0
		(+ (cdr (car v)) (deg (cdr v)))))


; Zkontrahuje dva vrcholy v jeden.
;
; v1, v2 = seznamy hran vrcholu
; co     = serazena dvojice indexu techto vrcholu
(define (kontr-vrch v1 v2 co)
	; dobrani zbytku hran z jednoho z vrcholu
	; nesmi se zapomenout na odebrani hrany mezi v1 a v2
	(define (zbytek v)
		(if (null? v)
			'()
			(let ((v-cislo (car (car v))))
				; je prvni prvek v hrana mezi v1 a v2?
				(if (or (= (car co) v-cislo) (= (cdr co) v-cislo))
					(zbytek (cdr v))
					(cons (car v) (zbytek (cdr v)))))))

	(cond
		((null? v1) (zbytek v2))
		((null? v2) (zbytek v1))
		(else
			(let ((v1-jm (caar v1))
				  (v2-jm (caar v2)))
				(cond
					; zapomeneme hrany mezi v1 a v2
					((or (= (car co) v1-jm) (= (cdr co) v1-jm)) (kontr-vrch (cdr v1) v2 co))
					((or (= (car co) v2-jm) (= (cdr co) v2-jm)) (kontr-vrch v1 (cdr v2) co))

					; hladove prochazeni hran (ktere jsou serazene vzestupne)
					((> v1-jm v2-jm) (cons (car v2) (kontr-vrch v1 (cdr v2) co)))
					((> v2-jm v1-jm) (cons (car v1) (kontr-vrch (cdr v1) v2 co)))

					; pokud z obou vrcholu vede hrana do jineho, secte se kapacita v nove
					(else
						(cons
							(cons v1-jm (+ (cdar v1) (cdar v2)))
							(kontr-vrch (cdr v1) (cdr v2) co))))))))


; Opravi seznam hran, tak ze spoji hrany vedouci do kontrahovanych vrcholu
; a ostatni ponecha.
;
; Vrati dvojici (seznam-hran . kap), kde kap je kapacita hrany (nebo nenulove)
; do druheho vrcholu.
;
; sous je usporadany seznam hran ke zpracovani (podle cisel cilu)
; co je usporadana dvojice cisel kontrahovanych vrcholu
; bool je akumulatorova promena (pri inicializovat na #f)
;      znaci zda jsme pri prochazeni sous uz narazili na prvni z vrcholu
(define (kontr-redukce sous co bool)
	(define (next)
		(let ((vysl (kontr-redukce (cdr sous) co bool)))
			(cons (cons (car sous) (car vysl)) (cdr vysl))))

	; Prozkouma zda/jakou ma kapacitu hrana do druheho a podle
	; toho nastavi hranu do prvniho vrch.
	;
	; p - hrana do prvniho vrcholu
	; next - zbytek vrcholu k prozkoumani
	(define (first-ok p next)
		; dal prohledavame s tim, ze sme hranu do prvniho nasli
		(let ((vysl (kontr-redukce next co #t))
			  (akt-kap (cdr p))
			  (akt     (car p)))

			(cons
				(cons akt (+ akt-kap (cdr vysl)))
				(car vysl))))

	(if (null? sous)
		(cons '() 0)

		(let ((akt (car (car sous)))
			  (prvni (car co))
			  (druhy (cdr co)))
			(cond
				; nasli sme prvni a ted i druhy
				; -> posilame v navratove hodnote kapacitu
				((and bool (= druhy akt))
					(cons
						(cdr sous) ; zbytek vrcholu
						(cdr (car sous)))) ; kapacita hrany do druheho vrcholu

				; mame prvni vrchol bez druhyho
				(bool (next))

				; aktualni je mensi nez prvni z vrcholu
				((> prvni akt) (next))

				; aktualni je vetsi nebo roven prvnimu
				; -> zjistime kapacitu do druheho a pripocteme
				((< prvni akt)
					; jakoby se nasel prvni z vrcholu
					(let ((vysl (first-ok (cons prvni 0) sous)))
						; nebyl pozdeji druhy vrchol?
						(if (or (not (null? (car vysl))) (= (cdr (car vysl)) 0))
							; nutne pro udrzeni invariantu, ze seznam nasledniku
							; je serazen
							(cons (cdr vysl) 0)) ; -> nic nepridame
							(cons vysl 1))) ; preradime hranu do zkontrahovaneho
				(else (cons (first-ok (car sous) (cdr sous)) 1))))))


; Slozeni hran vedoucich do zkontrahovanych vrcholu
; (nastavi na index mensiho z nich)
(define (kontr-zbytek g co)
	(map (lambda (v.hrany)
			(let ((vysl (kontr-redukce (cdr v.hrany) co #f)))
				; (car vysl) protoze vysl je (vysl-operace . hodnota)
				(cons (car v.hrany) (car vysl))))
		 g))


; Vrati serazenou dvojici cisel vrcholu.
; a, b jsou vrcholy i s hranami
(define (vrch-jmena a b)
	(if (> (car a) (car b))
		(cons (car b) (car a))
		(cons (car a) (car b))))


; Vytvori graf se zkontrahovanymi poslednimi dvema vrcholy
; legalniho usporadani.
;
; Pouzivat jen v pripade, ze v lu jsou aspon 3 vrcholy
; ma smysl taky jen pokud dostane uz linearizovane vrcholy
; (navic chce linearizovane pozpatku = ne podle definice)
(define (kontrahovat-graf lu)
	(let ((prvni (car lu))
		  (druhy (car (cdr lu))))
	(let ((usp-jmena (vrch-jmena prvni druhy)))

		(cons
			; zkontrahovany vrchol
			(cons
				(car usp-jmena)
				(kontr-vrch (cdr prvni) (cdr druhy) usp-jmena))

			; a zkontrahovany zbytek grafu
			(kontr-zbytek (cdr (cdr lu)) usp-jmena)))))


; Opravi priority ve fronte prvku v zavislosti na vytvarenem usporadani.
;
; f je fronta a sez je seznam vrcholu, kterym se ma priorita opravit.
; Kazdy vrchol je dvojice (cislo . +priorita), kde priorita odpovida
; kapacite hrany v grafu.
(define (opravit-priority f sez)
	(if (null? sez)
		f
		(opravit-priority (fronta-inc f (car sez)) (cdr sez))))


; Vraci legalni usporadani grafu.
;
; Ve tvaru (usporadani . pomocna-fronta) => na konci (usporadani . ())
(define (legal-usp g f)
	(if (null? g)
		(cons '() f)
		
		; zisk nove fronty a castecne vytvoreneho usporadani
		; po predchozich odebranich maxim
		(let ((leg-usp
				(legal-usp
					(cdr g)
					; pridani prvku do fronty
					(fronta-add f (prvek (caar g) 0 (cdar g))))))

		; odebrani maxima a prepocitani priorit
		; [f.max ve tvaru (zbytek-fronty . (smazane-max . jeho-priorita))]
		(let ((f.max (del-max (cdr leg-usp))))
		(cons
			(cons (car (cdr f.max)) (car leg-usp))
			(opravit-priority (car f.max) (p-sousedi (cdr f.max))))))))


(define (edge-conn-run fg)
	(cond
		; nema aspon dva vrcholy
		((or (null? fg) (null? (cdr fg))) 0)

		; ma prave dva vrcholy
		((null? (cdr (cdr fg))) (deg (cdr (car fg))))

		; ma li vice nez dva tak vytvorit legalni usporadani,
		; zjistit rez mezi poslednimi dvema vrcholy v usporadani
		; a porovnat s grafem vzniklim kontrahovanim techto vrcholu
		(else
			(let ((usp (car (legal-usp fg '()))))
				(min
					(deg (cdr (car usp)))
					(edge-conn-run (kontrahovat-graf usp)))))))


; preformatuje graf na format:
;   graf   = seznam vrcholu
;   vrchol = cislo vrcholu a list hran
;   hrana  = dvojice (cilovy vrchol . kapacita=1)
(define (format-graf g)
	(map
		(lambda (v)
			(cons (car v) (map (lambda (x) (cons x 1)) (cdr v))))
		 g))


(define (edge-connectivity g)
	; predformatovat graf pro lepsi dalsi praci
	(edge-conn-run (format-graf g)))


; ----------------------------------
; -- Testovaci struktury a funkce --
; ----------------------------------

(define g
	(list
		(list 1 2 5 6)    ;    2 -- 3
		(list 2 1 3 6 7)  ;   /|\  / \
		(list 3 2 4 7)    ;  1 | 7    4
		(list 4 3 5)      ;  |\|/  \ /
		(list 5 1 4 6 7)  ;  | 6 -- 5
		(list 6 1 2 5 7)  ;  '------'
		(list 7 2 3 5 6)));  = jasna hranova 2-souvislost

(define g2 ; plny graf na 5-ti vrcholech
	(list
		(list 1 2 3 4 5)
		(list 2 1 3 4 5)
		(list 3 1 2 4 5)
		(list 4 1 2 3 5)
		(list 5 1 2 3 4)))

(define g3
	(list
		(list 1 2)
		(list 2 2 3)
		(list 3 2 3 4)
		(list 4 2 3 4 5)
		(list 5 2 3 4 5 6)
		(list 6 2 3 4 5 6 7)
		(list 7 2 3 4 5 6 7 8)
		(list 8 2 3 4 5 6 7 8 9)
		(list 9 2 3 4 5 6 7 8 9 10)
		(list 10 2 3 4 5 6 7 8 9 10 11)
		(list 11 2 3 4 5 6 7 8 9 10 11 12)
		(list 12 2 3 4 5 6 7 8 9 10 11 12 13)
		(list 13 2 3 4 5 6 7 8 9 10 11 12 13 14)
		(list 14 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		(list 15 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
		(list 16 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
		(list 17 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
		(list 18 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
		(list 19 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
		(list 20 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
		(list 21 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)
		(list 22 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)
		(list 23 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)
		(list 24 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
		(list 25 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)
		(list 26 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27)
		(list 27 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
		(list 28 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)
		(list 29 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)
		(list 30 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)))

(define fg (format-graf g))

(define (p g)
	(if (null? g)
		(begin (newline) #t)
		(begin (write (car g)) (newline) (p (cdr g)))))

(define (run)
	(edge-connectivity g))	
