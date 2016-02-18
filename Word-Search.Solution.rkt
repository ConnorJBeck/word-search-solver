;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Word-Search Solution|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;; Word Search Solver

;; A word search is a square grid of letters of any given size, paired with a
;; list of string. Each string in the list can be found within the grid, by
;; finding the first letter and reading all of the letters in a single direction
;; until the word is completed.

;; It tries to find all of the strings in the grid.
;; Some sample solves can be found at the end of this document.

;; ====================
;; Data Definitions:


;; Letter is 1String[A-Z] (capital letters only)

;; Grid is (listof Letter) that has a length that is a perfect square
;; interp.
;;  A square of letters in rows and columns. If the length of the grid
;;  was 16, there would be 4 rows and 4 columns. Each square has a row and
;;  column number. The grid is represented as a flat list, and each position
;;  is represented with Pos.

(define-struct pos (x y))
;; Pos is (make-pos Natural Natural)
;; interp. A position of a letter in the grid, where
;;  - x is the x coordinate
;;  - y is the y coordinate, and 0, 0 is the top left.

;; Unit is (listof Pos)
;; interp. a list representing the positions of multiple letters in the
;;         grid in a row to form a word

;; WordList is (listof Word)
;; interp. a list where each string is composed only of Letter.

;; Word is String
;; interp. a string composed only of Letter.


;; ===================
;; Constants:

(define A "A")
(define B "B")
(define C "C")
(define D "D")
(define E "E")
(define F "F")
(define G "G")
(define H "H")
(define I "I")
(define J "J")
(define K "K")
(define L "L")
(define M "M")
(define N "N")
(define O "O")
(define P "P")
(define Q "Q")
(define R "R")
(define S "S")
(define T "T")
(define U "U")
(define V "V")
(define W "W")
(define X "X")
(define Y "Y")
(define Z "Z")


(define G4A (list A A A A
                  A A A A
                  A A A A
                  A A A A)) ;test of 4 wide

(define G1I (list I)) ;simplest case, still has a word

(define G2AM (list M A
                   A M)) ;very simple list with two words, "AM" and "MA"

(define G3RDM (list R A T
                    E M P
                    X D J)) ;3 large, mostly random                    

(define G5SEA (list W F B S W
                    E I R E A
                    T S E A T
                    W H A L E
                    B L U E R)) ; easy to solve

(define G10PICNIC (list G R A P E S P T B Y
                        I I S Q I A N E C D
                        L N S Q C U E G H R
                        C A J F A S N G E I
                        O P U C K A G S E N
                        O K I M E G Q J S K
                        K I C G V E G D E S
                        I N E B A S K E T P
                        E S B L A N K E T U
                        S A N D W I C H E S))

(define GFOODS (list S X X D L L Q T S B W N O Q B Q O E C S B Z C I B X P M S W
                     Z R D Y Q K O C T R P B A S N I C J E G R I R U Q X C E Q C
                     U B E H P M C B U O S K V V Y U I I I T O E A A M V L R O V
                     V K H B A H W L N W W Q Y C T O R U Z W C Z N W B P W R X T
                     N L L T M A N M L N S P Y T S R B K C V C I B F P Y T L N M
                     T R O B H U M J A R V P E R E G K X O E O W E A M Z E L K N
                     X E N Q X T C K W I E L I B B L Z M R D L C R K C Q X M Y N
                     S K S F W A H U Q C O Q E N T M H Q A H I M R X D X K W I J
                     G A G V N P S C C E V U M P A Z U V N D A A I Q U F J X A E
                     B I T Y S E T L G U L R E G U C N X G Q Q F E F R X U T M G
                     E J U T A P R L Z B Y S O N W X H U E O T X S I Q P B V Q G
                     A P K N G H A E W T E Y Q D L U S C S X J K C F A Y H L C S
                     N P A C O L R L B E V L Q B F B Q Q V L B E G M I I H E Z S
                     S M N A A L O B H U L L P Z Q A O N I U Q W N F B V A V Y N
                     C S M F L S E C Y S G Z Y Y X R Z A O Q E K T N Q Y Q O R I
                     T T W G A M H M I C P B I G E D P D M Q O B M O Q P C V R A
                     R K G E A X O E R I Q B X T K K X X V C G E Q D X O O B P R
                     V H O K Q O G N W E H J A I F T D Q L P G A N A H T R X E G
                     N S F Q F O I W D S T W D F E A M N V S N Z R K X A N U W E
                     O A T M E A L G F S S A L I X R O Y E S W Y K L Y T T P N L
                     X C S V L P E V L A W E W X M M O F Z T R V T R V O Y T Y O
                     O J P X I Q P E N F E Q C H L G Q U N O C V D Z W E T U C H
                     S B Q K E R Z A B L L J D A U W C L O R C E P S Q S Q D A W
                     E Q I E B M N O E B P G S R E C A C O R A I L X H R F A B L
                     U X X S A A L E M O N S T L H U S D V A S W C E W W Y P L A
                     W Y L I B N N M G X G N Q I Q Q B K U C K N D A R Y S G R F
                     C H I C K E N L Q G U D N R Y K O F X X X F U E L Y R E Y V
                     T P Y C H H G M Q Z B I N F I C O N H I X B B C Q Z A X Z O
                     P N K U V O N B M J V C O Z U T A P M U X S J U D N M Y O R
                     V P O F D L H M Z Z O H Y G O C I T B S Q T S A X N X E P G))



(define WA (list "A"
                 "AA"
                 "AAA"
                 "AAAA"))

(define WAf (list "A"
                  "AAAAAA"
                  "AA"))

(define WI (list "I"))

(define WRDM1 (list "RAT"))

(define WRDM2 (list "JMR"
                    "DMA"
                    "XMT"
                    "PME"
                    "EMP"
                    "TMX"
                    "AMD"
                    "RMJ")) ;test for every direction

(define WRDMf (list "RAT"
                    "RMA"))

(define WSEA (list "SEA"
                   "FISH"
                   "WHALE"
                   "WATER"
                   "WET"
                   "SEAL"
                   "BLUE"))

(define WPICNIC (list "BASKET"
                      "CHEESE"
                      "EGGS"
                      "NAPKINS"
                      "BLANKET"
                      "COOKIES"
                      "GRAPES"
                      "SANDWICHES"
                      "CAKE"
                      "DRINKS"
                      "JUICE"
                      "SAUSAGES"))

(define WFOODS (list "ALMONDS"
                     "BEANS"
                     "BROWNRICE"
                     "CELERY"
                     "CORN"
                     "EGGS"
                     "OATMEAL"
                     "SALMON"
                     "TOMATOES"
                     "WATERMELON"
                     "ZUCCHINI"
                     "APPLES"
                     "BLUEBERRIES"
                     "CARROTS"
                     "CHEESE"
                     "CRANBERRIES"
                     "LEMONS"
                     "ORANGES"
                     "SPINACH"
                     "WALNUTS"
                     "WHOLEGRAINS"
                     "BANANAS"
                     "BROCCOLI"
                     "CASHEWS"
                     "CHICKEN"
                     "CUCUMBERS"
                     "LETTUCE"
                     "POTATOES"
                     "TOFU"
                     "WATER"
                     "YOGURT"
                     "QUINOA"))
                     
(define WFOODSf (list "ALMONDS"
                      "BEANS"
                      "BROWNRICE"
                      "CELERY"
                      "CORN"
                      "EGGS"
                      "OATMEAL"
                      "SALMON"
                      "TOMATOES"
                      "WATERMELON"
                      "ZUCCHINI"
                      "APPLES"
                      "REDMEAT"
                      "BLUEBERRIES"
                      "CARROTS"
                      "CHEESE"
                      "CRANBERRIES"
                      "LEMONS"
                      "ORANGES"
                      "SPINACH"
                      "PINECONES"
                      "WALNUTS"
                      "WHOLEGRAINS"
                      "BANANAS"
                      "BROCCOLI"
                      "CASHEWS"
                      "CHICKEN"
                      "CUCUMBERS"
                      "LETTUCE"
                      "POTATOES"
                      "TOFU"
                      "WATER"
                      "YOGURT"
                      "QUINOA"))


;; ==================
;; Functions:

;; Grid WordList -> (listof Unit)
;; Produces a list of Units, where each Unit is a set of positions in the grid
;; that form a word from the word list. If a word cannot be found, an empty list
;; is produced in its place.
;; Assume: all grids are perfect squares
;; Assume: all Words in WordList are at least one Letter long

(check-expect (solve G4A empty) empty)
(check-expect (solve G1I (list "A")) (list (list)))
(check-expect (solve G1I WI) (list (list (make-pos 0 0))))
(check-expect (solve G4A WA) (list (list (make-pos 0 0))
                                   (list (make-pos 0 0)
                                         (make-pos 1 0))
                                   (list (make-pos 0 0)
                                         (make-pos 1 0)
                                         (make-pos 2 0))
                                   (list (make-pos 0 0)
                                         (make-pos 1 0)
                                         (make-pos 2 0)
                                         (make-pos 3 0))))
(check-expect (solve G4A WAf) (list (list (make-pos 0 0))
                                    (list)
                                    (list (make-pos 0 0)
                                          (make-pos 1 0))))
(check-expect (solve G3RDM WRDMf) (list (list (make-pos 0 0)
                                              (make-pos 1 0)
                                              (make-pos 2 0))
                                        (list)))
(check-expect (solve G3RDM WRDM2) (list (list (make-pos 2 2)
                                              (make-pos 1 1)
                                              (make-pos 0 0))
                                        (list (make-pos 1 2)
                                              (make-pos 1 1)
                                              (make-pos 1 0))
                                        (list (make-pos 0 2)
                                              (make-pos 1 1)
                                              (make-pos 2 0))
                                        (list (make-pos 2 1)
                                              (make-pos 1 1)
                                              (make-pos 0 1))
                                        (list (make-pos 0 1)
                                              (make-pos 1 1)
                                              (make-pos 2 1))
                                        (list (make-pos 2 0)
                                              (make-pos 1 1)
                                              (make-pos 0 2))
                                        (list (make-pos 1 0)
                                              (make-pos 1 1)
                                              (make-pos 1 2))
                                        (list (make-pos 0 0)
                                              (make-pos 1 1)
                                              (make-pos 2 2))))
(check-expect (solve G5SEA WSEA) (list (list (make-pos 3 0)
                                             (make-pos 3 1)
                                             (make-pos 3 2))
                                       (list (make-pos 1 0)
                                             (make-pos 1 1)
                                             (make-pos 1 2)
                                             (make-pos 1 3))
                                       (list (make-pos 0 3)
                                             (make-pos 1 3)
                                             (make-pos 2 3)
                                             (make-pos 3 3)
                                             (make-pos 4 3))
                                       (list (make-pos 4 0)
                                             (make-pos 4 1)
                                             (make-pos 4 2)
                                             (make-pos 4 3)
                                             (make-pos 4 4))
                                       (list (make-pos 0 0)
                                             (make-pos 0 1)
                                             (make-pos 0 2))
                                       (list (make-pos 3 0)
                                             (make-pos 3 1)
                                             (make-pos 3 2)
                                             (make-pos 3 3))
                                       (list (make-pos 0 4)
                                             (make-pos 1 4)
                                             (make-pos 2 4)
                                             (make-pos 3 4))))


;<template from blending of arbitrary arity tree, back tracking search,
; and generative recursion>

(define (solve g0 wl)
  (local [(define size (length g0))
          (define side (sqrt size))
          
          ;; WordList -> (listof Unit)
          (define (solve-grid wl)
            (cond [(empty? wl) empty]
                  [else
                   (cons (solve-word (first wl))
                         (solve-grid (rest wl)))]))
          
          
          ;; Word -> Unit
          (define (solve-word w)
            (search-word w (find-l-in-g (first-letter w))))
          
          
          ;; Letter -> (listof Pos)
          (define (find-l-in-g l)
            ;; n is Natural ; the current position in g0
            (local [(define (find-l g l n)
                      (cond [(empty? g) empty]
                            [else
                             (if (found-letter? (first g) l)
                                 (cons (n->pos n side)
                                       (find-l (rest g) l (add1 n)))
                                 (find-l (rest g) l (add1 n)))]))]
              (find-l g0 l 0)))
          
          
          ;; Word (listof Pos) -> Unit 
          (define (search-word w lop)
            (cond [(empty? lop) empty]
                  [else
                   (local [(define try (find-word w (first lop)))]
                     (if (not (false? try))
                         try
                         (search-word w (rest lop))))]))
          
          
          ;; Word Pos -> Unit or false
          (define (find-word w p)
            (search-units w (make-units p)))
          
          
          ;; Pos -> (listof Unit)
          (define (make-units p)
            (list (up-left-unit p)
                  (up-unit p)
                  (up-right-unit p)
                  (left-unit p)
                  (right-unit p)
                  (down-left-unit p)
                  (down-unit p)
                  (down-right-unit p)))
          
          
          ;; Pos -> Unit
          (define (up-left-unit p)    (build-unit sub1     sub1     p))
          (define (up-unit p)         (build-unit identity sub1     p))
          (define (up-right-unit p)   (build-unit add1     sub1     p))
          (define (left-unit p)       (build-unit sub1     identity p))
          (define (right-unit p)      (build-unit add1     identity p))
          (define (down-left-unit p)  (build-unit sub1     add1     p))
          (define (down-unit p)       (build-unit identity add1     p))
          (define (down-right-unit p) (build-unit add1     add1     p))
          
          
          ;; (Natural -> Natural) (Natural -> Natural) Pos -> Unit
          (define (build-unit c1 c2 p)
            (local [(define (build-unit p)
                      (if (invalid-pos? p)
                          empty
                          (cons p (build-unit (make-pos (c1 (pos-x p))
                                                        (c2 (pos-y p)))))))]
              (build-unit p)))
          
          
          ;; Pos -> Boolean
          (define (invalid-pos? p)
            (or (< (pos-x p) 0)
                (>= (pos-x p) side)
                (< (pos-y p) 0)
                (>= (pos-y p) side)))
          
          
          ;; Word (listof Unit) -> Unit or false
          (define (search-units w lou)
            (cond [(empty? lou) false]
                  [else
                   (local [(define try (search-unit w (first lou)))]
                     (if (not (false? try))
                         try
                         (search-units w (rest lou))))]))
          
          
          ;; Word Unit -> Unit or false
          (define (search-unit w u)
            (local [(define (prefix w u)
                      (cond [(empty? w) empty]
                            [(empty? u) false]
                            [else
                             (local [(define subs (prefix (rest w) (rest u)))]
                               (if (not (false? subs))
                                   (if (string=? (first w) (get-letter g0 (first u)))
                                       (cons (first u) subs)
                                       false)
                                   false))]))]
              (prefix (explode w) u)))
          
          
          
          ;; Word Unit -> Unit or false
          #;
          (define (search-unit w u)
            (local [(define wl (string-length w))
                    (define ul (length u))
                    
                    (define (new-list n)
                      (list-ref u n))
                    
                    (define (search-unit count)
                      (cond [(>= count wl) (build-list wl new-list)]
                            [(>= count ul) false]
                            [else
                             (if (letter-match? count)
                                 (search-unit (add1 count))
                                 false)]))
                    
                    (define (letter-match? count)
                      (string=? (substring w count (add1 count))
                                (get-letter g0 (list-ref u count))))]
              (search-unit 0)))
          
          
          
          ;; Word Unit -> Unit or false
          #;
          (define (search-unit w u)
            (cond [(string=? w "") empty]
                  [(empty? u) false]
                  [else
                   (local [(define subs (search-unit (rest-str w) (rest u)))]
                     (if (not (false? subs))
                         (letter-match w u subs)
                         false))]))
          
          
          ;; Word Unit Unit -> Unit or false
          #;
          (define (letter-match w u subs)
            (if (string=? (first-letter w) (get-letter g0 (first u)))
                (cons (first u) subs)
                false))]
    
    (solve-grid wl)))



;; String - > Letter
;; produces just the first letter of a string
;; Assume: All strings passed in will be at least 1 long

(check-expect (first-letter "cat") "c")

;<template from simple atomic>

(define (first-letter str)
  (substring str 0 1))



;; Letter Letter -> Boolean
;; determines if the letters are the same

(check-expect (found-letter? "A" "C") false)
(check-expect (found-letter? "D" "D") true)

;<template from two simple atomics>

(define (found-letter? l1 l2)
  (string=? l1 l2))



;; String -> String
;; produces the str without the first letter

(check-expect (rest-str "cat") "at")
(check-expect (rest-str "t") "")

;<template from atomic non-distinct>

(define (rest-str str)
  (substring str 1))



;; Natural Natural -> Pos
;; Produces a pos based on the side-length of the grid
(check-expect (n->pos 5 3) (make-pos 2 1))

;<template from 2 atomic non-distincts>

(define (n->pos n side)
  (make-pos (remainder n side)
            (quotient n side)))



;; Grid Pos -> Letter
;; Produce value at given position on board.
(check-expect (get-letter G5SEA (make-pos 0 4)) "B")

;<template from Pos with added Grid parameter>

(define (get-letter g p)
  (local [(define s (sqrt (length g)))
          (define x (pos-x p))
          (define y (pos-y p))]
    (list-ref g (+ (* y s) x))))





;; =================
;; Render Function

(define CORRECT (circle 10 "outline" "red"))
(define BG (square 20 "solid" "white"))
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")



;; Grid WordList -> Image
;; renders a word search with all of the letters that are in words circled and word list beside

(check-expect (render G1I empty)
              (overlay (text "I" TEXT-SIZE TEXT-COLOR) BG))
(check-expect (render G1I WI)
              (beside (overlay CORRECT (text "I" TEXT-SIZE TEXT-COLOR) BG)
                      (square 30 "solid" "white")
                      (local [(define word (text "I" (- TEXT-SIZE 2) TEXT-COLOR))]
                        (add-line word
                                  0
                                  (/ (image-height word) 2)
                                  (image-width word)
                                  (/ (image-height word) 2)
                                  "red"))))
(check-expect (render G3RDM WRDMf)
              (beside (above (beside (overlay CORRECT (text "R" TEXT-SIZE TEXT-COLOR) BG)
                                     (overlay CORRECT (text "A" TEXT-SIZE TEXT-COLOR) BG)
                                     (overlay CORRECT (text "T" TEXT-SIZE TEXT-COLOR) BG))
                             (beside (overlay (text "E" TEXT-SIZE TEXT-COLOR) BG)
                                     (overlay (text "M" TEXT-SIZE TEXT-COLOR) BG)
                                     (overlay (text "P" TEXT-SIZE TEXT-COLOR) BG))
                             (beside (overlay (text "X" TEXT-SIZE TEXT-COLOR) BG)
                                     (overlay (text "D" TEXT-SIZE TEXT-COLOR) BG)
                                     (overlay (text "J" TEXT-SIZE TEXT-COLOR) BG)))
                      (square 30 "solid" "white")
                      (above (local [(define word (text "RAT" (- TEXT-SIZE 2) TEXT-COLOR))]
                               (add-line word
                                         0
                                         (/ (image-height word) 2)
                                         (image-width word)
                                         (/ (image-height word) 2)
                                         "red"))
                             (text "RMA" (- TEXT-SIZE 2) TEXT-COLOR))))

;<template from generative recursion>

(define (render g los)
  ;; p is Pos ; current location in g
  
  (local [(define solved (solve g los))
          (define corrects (collapse solved))
          (define side (sqrt (length g)))
          
          (define (render-both p)
            (if (empty? los) 
                (render-board p)
                (beside (render-board p)
                        (square 30 "solid" "white")
                        (render-wl los solved))))

          (define (render-wl los slvd)
            (cond [(empty? los) empty-image]
                  [else
                   (above (render-word (first los) (first slvd))
                          (render-wl (rest los) (rest slvd)))]))
          
          (define (render-word str lop)
            (local [(define word-image (text str (- TEXT-SIZE 2) TEXT-COLOR))]
              (if (empty? lop)
                  word-image
                  (add-line word-image
                            0
                            (/ (image-height word-image) 2)
                            (image-width word-image)
                            (/ (image-height word-image) 2)
                            "red"))))
          
          (define (render-board p)
            (if (>= (pos-y p) side)
                empty-image
                (above (render-row p)
                       (render-board (make-pos (pos-x p)
                                               (add1 (pos-y p)))))))
          
          (define (render-row p)
            (if (>= (pos-x p) side)
                empty-image
                (beside (render-square p)
                        (render-row (make-pos (add1 (pos-x p))
                                              (pos-y p))))))
          
          (define (render-square p)
            (overlay (correct? p)
                     (text (get-letter g p) TEXT-SIZE TEXT-COLOR)
                     BG))
          
          (define (correct? p)
            (if (member? p corrects)
                CORRECT
                empty-image))]
    
    (render-both (make-pos 0 0))))


;; (listof Unit) -> Unit
;; collapses all pos's in the (listof Unit) into one large list

(check-expect (collapse (list (list 0 1) (list 5 6)))
              (list 0 1 5 6))
(check-expect (collapse (solve G4A WA))
              (list (make-pos 0 0)
                    (make-pos 0 0)
                    (make-pos 1 0)
                    (make-pos 0 0)
                    (make-pos 1 0)
                    (make-pos 2 0)
                    (make-pos 0 0)
                    (make-pos 1 0)
                    (make-pos 2 0)
                    (make-pos 3 0)))

;<template from call to foldr>

(define (collapse lou)
  (foldr append empty lou))

;; This call will show a completed word search
;(render GFOODS WFOODS)

;; This call will show a word search where two words could not be found (because they do not exist in the puzzle)
;(render GFOODS WFOODSf)
