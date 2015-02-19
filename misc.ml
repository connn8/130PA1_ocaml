(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
 * (sumList l) is the sum of the digits in int list l   
*) 

let rec sumList l =
	match l with
	|[] -> 0
	|head::tail -> head + sumList tail;;


(* digitsOfInt : int -> int list 
 * (digitsOfInt n) is the list of digits of n in the order in
 * which they appear in n   
 *)

let rec digitsOfInt n = 
	(*gets the last digit*)
	let last_dig = abs(n mod 10) in  
	(*gets all the previous digits*)
	let rem_dig = abs(n/10) in
	match rem_dig with
	|0 -> [last_dig]
	|_ -> digitsOfInt rem_dig @ [last_dig];;    (*recursively appends the digits to the beggining of list*)


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required tto obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
 * (additivePersistence n) is the additive persisntence of n, the number of 
 * additions of the digits of n and the derived numbers thereafter required
 * to obtain a single digit. 
*)

let rec additivePersistence n = 
	(*puts digits of n in a list and then sums them*)
	let nList = digitsOfInt (abs n) in
	let nSum = sumList nList in
	(*checks to see if sum is one digit, if not adds one and calls method recursively*)
	match nSum < 10 with
	|true -> 1
	|false -> 1 + additivePersistence nSum;; 

(* digitalRoot : int -> int
 * (digitalRoot n) is the single digit obtained after computing the additive
 * persistence of n
*)
let rec digitalRoot n = 
	let nList = digitsOfInt (abs n) in
	let nSum = sumList nList in
	match nSum < 10 with
	|true -> nSum
	|false -> digitalRoot nSum;; 

(* listReverse : a' list -> a' list 
* (listReverse l) is a list of the elements of l in the reversed order
*)
let rec listReverse l =
	match l with
	|[] -> []
	|head::tail -> listReverse tail @ [head];;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool
* (palindrom w) is a boolean stating weather the string w is a palindrome
*)
let palindrome w =  
	(*puts chars of w in a list and then reverses it*)
	let charList = explode w in
	let revList = listReverse charList in
	match charList = revList with
	|false -> false
	|true -> true;;

