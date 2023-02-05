#lang shrubbery

define (add-drawing p):
  define drawer: make-pict-drawer p
  new canvas%:
    parent: f
    style: quote (border)
    paint-callback:
      lambda (self dc):
        drawer dc 0 0

displayln:
  string-append: "hello "
                 name
                 "!"

list 1 2 3 4 5 6

list: 1; 2; 3
      4; 5; 6

overlay/offset:
  rectangle 100 10 "solid" "blue"
  10; 10
  rectangle 10 100 "solid" "red"

standard-cat: 100; 90
              ~happy? #true

standard-cat:
  100; 90
  ~happy? #true

define drawer (make-pict-drawer p)

define (display-excitement str):
  format: "I'm SO EXCITED about ~a!!!"
          string-upcase str

define (greeter1 name):
  let ((to-say:
          format: "Hey there ~a! :D"
                  name)):
    displayln to-say

define (greeter2 name):
  let [to-say:
         format: "Hey there ~a! :D"
                 name]:
    displayln to-say

for [pet: quote ("cat" "dog" "horse")]:
  printf "I love my ~a!\n" pet

define (counting-letters-song letters):
  for [letter: letters,
       number: in-naturals 1]:
    printf "I like ~a, it's number ~a!" letter number
    (newline)
  displayln "Singing a letters song!"

let* [animal: "dog",
      noise: "barks",
      player-hears:
        format "the ~a says: ~a!!!" animal noise]:
  displayln player-hears

define (double x):
  x * 2

define (squared-minus-one x):
  (x * x) - 1

1 + 2 + (9 / 3)

a b_1 b_2 b_3:
  c_1
  c_2
  c_3

(a b_1 b_2 b_3:
   c_1
   c_2
   c_3)

[a_1 b_1_1 b_1_2 b_1_3:
  c_1_1
  c_1_2
  c_1_3,
 a_2 b_2_1 b_2_2 b_2_3:
  c_1_1
  c_1_2
  c_1_3,
 a_3 b_3_1 b_3_2 b_3_3:
  c_3_1
  c_3_2
  c_3_3]

a b_1
d

a: b_1:
     d

a: b_1
   c_1:
     d



