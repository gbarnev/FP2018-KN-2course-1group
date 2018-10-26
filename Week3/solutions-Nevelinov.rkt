#lang racket
;Задача 1. Да се дефинира функцията (perfect-number? n), която проверява
;дали числото n e съвършено, т.е. дали е равно на сбора на делителите си.
(define (perfect-number? n)
  (define (sum-of-divisors sum divisor)
    (cond
      [(= divisor n) sum]
      [(= (remainder n divisor) 0) (sum-of-divisors (+ sum divisor) (+ divisor 1))]
      [else (sum-of-divisors sum (+ divisor 1))]))
  (= n (sum-of-divisors 0 1)))

;Задача 2. Да се дефинира функцията (inc-digits? n), която проверява дали цифрите на числто n са подредени в нарастващ ред.
;а)
(define (int-digits? n)
  (define (help number prev)
    (cond
      [(= number 0) #t]
      [(<= (remainder number 10) prev) (help (quotient number 10) (remainder number 10))]
      [else #f]))
  (help (quotient n 10) (remainder n 10)))

;б)
(define (increasing? n)
  (or (< n 10)
      (and (<= (remainder (quotient n 10) 10) (remainder n 10))
           (increasing? (quotient n 10)))))

;Задача 3. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n.
(define (sum x n)
  (define (helper sum current counter)
    (if(= counter n)
       (+ sum current)
       (helper (+ sum current) (* x current) (+ counter 1))))
  (helper 1 x 1))
       

;Задача 4. Да се реши задача 4, чрез използване на не повече от n умножения. - задача 3
;виж горе

;Задача 5. Дефинирайте следните функции:

;a). (my-identity x), функцията идентитет: връща каквото и дадете.
(define (my-identity x)
  (lambda (x) x) x)

;(my-identity 5) -> 5


;б). (my-compose f g), която връща композицията на функциите f и g.
(define (f x)
  (* x 10))
(define (g x)
  (+ x 2))

(define (my-compose f g)
  (define (helper f g x)
    (f (g x)))
  (lambda (x) (helper f g x)))

;(my-compose) ще върне като резултат функция, към там зи функция подаваме аргумент 5
;((my-compose f g) 5) -> 70


;в). (my-negate p?), която приема предикат p? и връща предиката (not p?).
(define (my-negate p?)
  (lambda (x) (not (p? x))))

;(my-negate) ще върне като резултат функция, към тази функция подаваме аргумент 5
;((my-negate odd?) 5); -> #f


;г). (my-curry f x), която приема 4ри аргументна (многоаргументна) функция f и първи аргумент x и връща функцията получена от частичното прилагане на x върху f.
;(define (my-curry f x)
;Haskell


;д). (repeatf f n), която връща n-кратната композиция на функцията f.
(define (repeatf f n)
  (if(= n 0)
     my-identity
     (my-compose f (repeatf f (- n 1)))))

;Ламбда изразът се прилага 5 пъти над аргумент 1
;((repeatf (lambda (arg) (+ arg 1)) 5) 1) -> 6
