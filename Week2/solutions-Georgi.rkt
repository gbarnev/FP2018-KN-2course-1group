#lang racket

;Задача 1. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.

(define (mymaxdivisor x)
  (define (helper x num)
    (if (= 0 (remainder x num))
        num
        (helper x (- num 1))))
  (helper x (- x 1)))

;Задача 2. Да се дефинира функцията leap-year? year, която проверява дали годината year е високосна.

(define (leap-year? year)
  (cond
    [(= 0 (remainder year 400)) #t]
    [(= 0 (remainder year 100)) #f]
    [(= 0 (remainder year 4)) #t]
    [else #f]))

;Задача 3. Да се дефинира функцията valid-date? day month year, която връща дали датата (day, month, year) e валидна.

(define (valid-date? day month year)
  (cond [(or (< day 1) (< month 1) (< year 0) (> month 12)) #f]
        [(and (leap-year? year)(= month 2))(<= day 29)]
        [(= month 2)(<= day 28)]
        [(and (<= month 7)(= 1 (remainder month 2)))(<= day 31)]
        [(<= month 7) (<= day 30)]
        [(and (>= month 8)(= 1 (remainder month 2)))(<= day 30)]
        [else (<= day 31)]))

;Задача 4. Да се дефинира функция, която намира сумата на нечетните числа в затворения интервал [a, b].

(define (sum-odd a b)
  (define (helper curr until)
    (if (> curr until)
        0
        (+ curr (helper (+ curr 2) until))))
  (if (= 1 (remainder a 2))
      (helper a b)
      (helper (+ a 1) b)))
            
;Задача 5. Да се дефинира предикат, който проверява дали естественото число n е просто.

(define (is-prime n)
  (define (helper i n)
    (cond [(> (* i i) n) #t]
          [(= 0 (remainder n i)) #f]
          [else (helper (+ i 1) n)]))
  (helper 2 n))

;Задача 6. Да се дефинира функция, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.

(define (is-palindrome n)
  (define (helper i rev num)
    (if (<= i 0)
        (= num rev)
        (helper (quotient i 10) (+ (* rev 10) (remainder i 10)) num)))
  (helper n 0 n))

;Задача 7. Да се дефинира функция, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число.

(define (divisors-count n)
  (define (helper i n count)
    (cond [(> (* i i) n) count]
          [(and (= 0 (remainder n i)) (= i (/ n i))) (helper (+ i 1) n (+ count 1))]
          [(= 0 (remainder n i)) (helper (+ i 1) n (+ count 2))]
          [else (helper (+ i 1) n count)]))
  (helper 1 n 0))
