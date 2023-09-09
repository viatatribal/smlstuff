(* This program checks if a given number is prime or not
 * It assumes that n > 10, as the first four primes are trivial,
 * and that n is not even, as 2 is the only even prime number.
 *)


local
  open LargeInt;
  fun intSqrt n =
      fromInt
          (Real.floor
               (Real.Math.sqrt(Real.fromLargeInt n)));
in
  (* Check if n is divisible by n prime less than n *)
  fun isPrime n  =
      let
        fun prime' f r =
            if f > r
            then true
            else if n mod f = 0
            then false
            else if n mod (f+2) = 0
            then false
            else prime' (f+6) r
      in
        if n mod 3 = 0
        then false
        else prime' 5 (intSqrt n)
      end
end;
