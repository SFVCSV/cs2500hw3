;; Purpose: Design Recipe practice, now with structured data.

(require 2htdp/image)

;;! Instructions
;;! 1. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 2. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).
;;! 3. You must follow the Style Guide:
;;!    https://pages.github.khoury.northeastern.edu/2500/2024F/style.html
;;! 4. You must submit working code. In DrRacket, ensure you get on errors
;;!    when you click Run. After you submit on Gradescope, you'll get instant
;;!    feedback on whether or Gradescope can run your code, and your code must
;;!    run on Gradescope to receive credit from the autograder. Any problems
;;!    reported by the autograder can be corrected and you can resubmit as many
;;!    times as you want before the deadline.

;;! Problem 1

;; Consider the following data definition and interpretation.

;;! A Time is a (make-time Number Number Number)
;;! Represents the time of day where:
;;! – hours is between 0 and 23
;;! – minutes is between 0 and 59
;;! – seconds is between 0 and 59

;;! Part A
;; Complete the two remaining parts of the data design for Time.

(define (time hours minutes seconds)
  (time hours minutes seconds))
(check-expect (time 10 20 30) (time 10 20 30))
(check-expect (time 0 59 59) (time 0 59 59))
(check-expect (time 23 0 0) (time 23 0 0))


(define (time-seconds a-time)
  (time-seconds a-time))
(check-expect (time-seconds (time 10 20 30)) 30)
(check-expect (time-seconds (time 0 59 59)) 59)
(check-expect (time-seconds (time 23 0 0)) 0)


(define (time-minutes a-time)
  (time-minutes a-time))
(check-expect (time-minutes (time 10 20 30)) 20)
(check-expect (time-minutes (time 0 59 59)) 59)
(check-expect (time-minutes (time 23 0 0)) 0)


(define (time-hours a-time)
  (time-hours a-time))
(check-expect (time-hours (time 10 20 30)) 10)
(check-expect (time-hours (time 0 59 59)) 0)
(check-expect (time-hours (time 23 0 0)) 23)


;;! Part B
;; Design a function called tick that adds one second to a Time.

(define (tick t)
  (if (= (time-seconds t) 59)
      (if (= (time-minutes t) 59)
          (if (= (time-hours t) 23)
              (time 0 0 0)
              (time (+ 1 (time-hours t)) 0 0))
          (time (time-hours t) (+ 1 (time-minutes t)) 0))
      (time (time-hours t) (time-minutes t) (+ 1 (time-seconds t)))))
(check-expect (tick (time 10 20 30)) (time 10 20 31)) 
(check-expect (tick (time 10 20 59)) (time 10 21 0)) 
(check-expect (tick (time 10 59 59)) (time 11 0 0))  
(check-expect (tick (time 23 59 59)) (time 0 0 0))   
(check-expect (tick (time 0 0 0)) (time 0 0 1))


;;! Part C

;; Design a function called time->image that draws an analog clock face with
;; three hands. (The hour hand is shortest and the minute and second hand should
;; be different.)
;;
;; See the link below for a refresher on how an analog clock works
;; https://en.wikipedia.org/wiki/Clock_face
;; Note: The hour hand does not need to base it's position on the minute hand
;; for this assignment

(define clock-radius 100)

(define (hours->angle hours)
  (- (* 30 (modulo hours 12))))
(check-expect (hours->angle 0) 0)
(check-expect (hours->angle 3) -90)
(check-expect (hours->angle 12) 0)

(define (minutes->angle minutes)
  (- (* 6 minutes)))
(check-expect (minutes->angle 0) 0)
(check-expect (minutes->angle 15) -90)
(check-expect (minutes->angle 30) -180)

(define (seconds->angle seconds)
  (- (* 6 seconds)))
(check-expect (seconds->angle 0) 0)
(check-expect (seconds->angle 15) -90)
(check-expect (seconds->angle 45) -270)

(define (time->image t)
  (place-image
   (rotate (hours->angle (time-hours t)) (line 50 "black"))
   100 100
   (place-image
    (rotate (minutes->angle (time-minutes t)) (line 80 "blue"))
    100 100
    (place-image
     (rotate (seconds->angle (time-seconds t)) (line 100 "red"))
     100 100
     (circle clock-radius "outline" "black")))))

(check-expect (time->image (time 3 15 45)) ; 3:15:45
              (place-image
               (rotate -90 (line 50 "black"))
               100 100
               (place-image
                (rotate -90 (line 80 "blue"))
                100 100
                (place-image
                 (rotate -270 (line 100 "red"))
                 100 100
                 (circle 100 "outline" "black")))))

(check-expect (time->image (time 6 30 0)) ; 6:30:00
              (place-image
               (rotate -180 (line 50 "black"))
               100 100
               (place-image
                (rotate -180 (line 80 "blue"))
                100 100
                (place-image
                 (rotate 0 (line 100 "red"))
                 100 100
                 (circle 100 "outline" "black")))))

(check-expect (time->image (time 12 0 0)) ; 12:00:00
              (place-image
               (rotate 0 (line 50 "black"))
               100 100
               (place-image
                (rotate 0 (line 80 "blue"))
                100 100
                (place-image
                 (rotate 0 (line 100 "red"))
                 100 100
                 (circle 100 "outline" "black")))))


;;! Problem 2

;;! Part A

;; You are designing a registration system for a competition. Design a data definition
;; called Attendee that represents a person attending the competition. An Attendee
;; should have a name, gender, email, NUID, and whether they are competing (rather than just
;; observing).

(define-struct attendee (name gender email nuid competing))

(check-expect (make-attendee "Alice" "female" "alice@northeastern.edu" "0000000001" false) 
              (make-attendee "Alice" "female" "alice@northeastern.edu" "0000000001" false))
(check-expect (make-attendee "Bob" "male" "bob@gmail.com" "0000000002" true) 
              (make-attendee "Bob" "male" "bob@gmail.com" "0000000002" true))
(check-expect (make-attendee "Ruoqi" "female" "Ruoqi@northeastern.edu" "002860225" true) 
              (make-attendee "Ruoqi" "female" "Ruoqi@northeastern.edu" "002860225" true))

;;! Part B

;; Design a function called is-nu-email? that takes an Attendee and returns true if
;; the email ends with `@northeastern.edu`.

(define (is-nu-email? attendee)
  (equal? (substring (attendee-email attendee)
                     (- (string-length (attendee-email attendee)) 18))"@northeastern.edu"))

(check-expect (is-nu-email? (make-attendee "Alice" "female" "alice@northeastern.edu" "0000000001" false)) true)
(check-expect (is-nu-email? (make-attendee "Bob" "male" "bob@gmail.com" "0000000002" true)) false)
(check-expect (is-nu-email? (make-attendee "Ruoqi" "female" "Ruoqi@northeastern.edu" "002860225" true)) true)


;;! Part C

;; Design a function called mark-competing that takes an Attendee and marks them
;; as competing. This should only work if `is-nu-email?` returns true,
;; otherwise, leave the field unchanged.

(define (mark-competing attendee)
  
  (if (is-nu-email? attendee)
      (make-attendee (attendee-name attendee)
                     (attendee-gender attendee)
                     (attendee-email attendee)
                     (attendee-nuid attendee)
                     true)
      attendee))

(check-expect (mark-competing (make-attendee "Alice" "female" "alice@northeastern.edu" "0000000001" false)) 
              (make-attendee "Alice" "female" "alice@northeastern.edu" "0000000001" true))

(check-expect (mark-competing (make-attendee "Bob" "male" "bob@gmail.com" "0000000002" false)) 
              (make-attendee "Bob" "male" "bob@gmail.com" "0000000002" false))

(check-expect (mark-competing (make-attendee "Ruoqi" "female" "Ruoqi@northeastern.edu" "002860225" false)) 
              (make-attendee "Ruoqi" "female" "Ruoqi@northeastern.edu" "002860225" true))

;;! Part D
;;
;; In the next problem, you will have to _print_ a badge -- i.e., turn an
;; Attendee into an Image. In this problem, we will consider privacy, and
;; whether there is any information that we put it the Attendee data definition
;; that is not necessary. A core privacy principle is to collect as little
;; personal data as possible, focusing on the information directly needed for
;; the task at hand. In minimizing data collection, we reduce the risk of
;; privacy violations or data leaks, since if we do not have data, we cannot
;; accidentally leak it.
;;
;; With these data principles in mind, design a new data definition called
;; Attendee2 that stores less personal data. Compared to the original data
;; definition, the new data definition should *eliminate* at least one field and
;; *modify* at least one field to make it more precise.
;;
;; You new definiton will need to be used for badge printing, so think carefully
;; about what you remove.

;; Design a new data definition that eliminates at least one field from the previous
;; definition and modifies at least one other field.

(define-struct attendee2 (name gender email nuid competing))

(define (shorten-nuid full-nuid)
  (substring full-nuid (- (string-length full-nuid) 4)))

(check-expect (shorten-nuid "0000000001") "0001")
(check-expect (shorten-nuid "1234567890") "7890")
(check-expect (shorten-nuid "002860225") "0225")

(define (new-attendee2 name gender email full-nuid competing)
  (new-attendee2 name gender email (shorten-nuid full-nuid) competing))

(check-expect (new-attendee2 "Alice" "F" "alice@northeastern.edu" "0000000001" true) 
              (new-attendee2 "Alice" "F" "alice@northeastern.edu" "0001" true))

(check-expect (new-attendee2 "Bob" "M" "bob@gmail.com" "000000002" false) 
              (new-attendee2 "Bob" "M" "bob@gmail.com" "0002" false))

(check-expect (new-attendee2 "Ruoqi" "F" "charlie@northeastern.edu" "002860225" true) 
              (new-attendee2 "Ruoqi" "female" "Ruoqi@northeastern.edu" "0225" true))

;;! Part E

;; In order to answer this question, please watch this short (5min) video:
;; https://northeastern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=55828b6f-b114-4320-8a7c-b1f000c2d848

;; Write a short email memo to the other members of the conference team explaining
;; each change you made and justifying each change with a reason for why
;; your version is better (one to two sentences per change). At least one
;; of the justifications should reference a privacy concept from the video
;; (privacy as ability to control information; intimate privacy; privacy
;; as a social good; and privacy as minimizing information collection).

;; I have made the following changes to the Attendee data structure to better align with privacy principles and improve the system:
;; Gender Field Simplification: I simplified the gender field to a single character to reduce the amount of personal information collectedAccording tp Danielle
;; Citron's theory of intimate privacy, simplifying the gender and NUID fields is a better design for protecting personal information.
;; Citron emphasizes that intimate details, such as gender and identification numbers, are sensitive and should be shared minimally to maintain control
;; over personal boundaries. By reducing gender to simple markers and truncating NUIDs to the last four digits, we limit unnecessary exposure of intimate data,
;; thus respecting the individual's right to control how much private information they share, aligning with the principle of minimizing data collection.

;;! Part F
;;
;; Design a function `print-badge` that takes, as input an Attendee2 and returns
;; an Image. It should display information that you would want to show on a
;; printed badge for a competition.

(define (print-badge attendee2)
  (above 
   (text (attendee2-name attendee2) 20 "black")
   (text (string-append "NUID: " (attendee2-nuid attendee2)) 15 "red")
   (text (if (attendee2-competing attendee2) "Competing" "Observing") 15 "black")))

(check-expect (print-badge (make-attendee2 "Alice" "F" "alice@northeastern.edu" "0001" true)) 
              (above 
               (text "Alice" 20 "black")
               (text "NUID: 0001" 15 "red")
               (text "Competing" 15 "black")))

(check-expect (print-badge (make-attendee2 "Bob" "M" "bob@gmail.com" "7890" false)) 
              (above 
               (text "Bob" 20 "black")
               (text "NUID: 7890" 15 "red")
               (text "Observing" 15 "black")))

(check-expect (print-badge (make-attendee2 "Charlie" "N" "charlie@northeastern.edu" "2100" true)) 
              (above 
               (text "Ruoqi" 20 "black")
               (text "NUID: 0225" 15 "red")
               (text "Competing" 15 "black")))
