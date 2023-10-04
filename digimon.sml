use "digimondata.sml";

(* string -> string *)
(* removes 2/3 initials value from a string *)
(* it's used to remove the inital "xx." characters *)
fun strprefrem str =
    if String.sub (str, 2) = #"."
    then String.implode (List.drop (String.explode str, 3))
    else String.implode (List.drop (String.explode str, 2));

(* status1{1,2} -> string *)
(* returns a string without any "" from status1 or status2 *)
fun clearst ((a,b,c,d,e,f)) =
    String.concat
        (List.filter (fn x => x <> "\n")
                     (List.map (fn x => x ^ "\n") [a,b,c,d,e,f]));

(* digimon -> string *)
(* returns the string formed from a digimon requirements *)
fun reqstring ((st1,st2,st3) : digimon) =
    let val str = clearst (st1) ^ clearst (st2)
    in
      if st3 = "" then str
      else str ^ st3
    end;


(* digilevel -> string *)
(* returns a string from a digilevel *)
fun levelstring x =
    let val str = (String.concat o (List.map (fn x => x ^ "\n")))
    in
      case x of
          Fresh(y)    => "Fresh:\n" ^ str y
        | Training(y) => "Training:\n" ^ str y
        | Rookie(y)   => "Rookie:\n" ^ str y
        | Champion(y) => "Champion:\n" ^ str y
        | Ultimate(y) => "Ultimate:\n" ^ str y
                   end;


(* digilevel -> list *)
(* returns a list from a digilevel *)
fun levellist dl =
    case dl of
       Fresh(x)    => x
     | Training(x) => x
     | Rookie(x)   => x
     | Champion(x) => x
     | Ultimate(x) => x;

(* list string -> string *)
(* returns a string from a list of string *)
fun evostring x =
    String.concat (List.map (fn x => x ^ "\n") x);

(* unit -> unit *)
(* prints explanation of requirements *)
fun help () =
    ( TextIO.output(TextIO.stdOut, "You only need to satisfy 3 of the 4 properties:\n")
    ; TextIO.output(TextIO.stdOut, "1. HP-MP-Offense-Defense-Speed-Brains.\n")
    ; TextIO.output(TextIO.stdOut, "2. Care Mistakes.\n")
    ; TextIO.output(TextIO.stdOut, "3. Weight.\n")
    ; TextIO.output(TextIO.stdOut, "4. Discipline-Techniques-Battles-Happines-Extra Requirement.\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> unit *)
(* prints the main screen of the program *)
fun prelude () =
    ( TextIO.output(TextIO.stdOut,"Digimon Evo Tool!\n")
    ; TextIO.output(TextIO.stdOut,"This program shows the requirement to digivolve")
    ; TextIO.output(TextIO.stdOut," one digimon to another!\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> unit *)
(* prints an error indicating wrong number *)
fun errormsg (min, max) =
    ( TextIO.output(TextIO.stdOut, "Wrong number.\n")
    ; TextIO.output(TextIO.stdOut, "Must be a number between ")
    ; TextIO.output(TextIO.stdOut, Int.toString min)
    ; TextIO.output(TextIO.stdOut, " and ")
    ; TextIO.output(TextIO.stdOut, Int.toString max)
    ; TextIO.output(TextIO.stdOut, "\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> digilevel * int *)
(* user input the level [by number] he wants *)
fun levelselector () =
    case valOf (TextIO.inputLine(TextIO.stdIn)) of
       "1\n" => (fresh, 4)
     | "2\n" => (training, 4)
     | "3\n" => (rookie, 9)
     | "4\n" => (champion, 30)
     | "5\n" => (ultimate, 15)
     |  _    => ( errormsg(1,5); levelselector());


(* int int -> int *)
(* user input the digimon he wants between numbers min and max *)
fun pickdigimon (min, max) =
    let val n =
            valOf(Int.fromString(valOf(TextIO.inputLine(TextIO.stdIn))))
    in
      if n < min orelse n > max
      then (errormsg(min,max); pickdigimon (min,max))
      else n
    end;


(* unit -> unit *)
(* asks the user to select one of levels *)
fun whatlevel () =
    ( TextIO.output(TextIO.stdOut, "Select a level (enter a number):\n")
    ; TextIO.output(TextIO.stdOut, "1.Fresh\n2.Training\n3.Rookie\n")
    ; TextIO.output(TextIO.stdOut, "4.Champion\n5.Ultimate\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* string -> unit *)
(* asks the user what digimon he wants *)
fun whatdigimon (str,digistr) =
    ( TextIO.output(TextIO.stdOut, str)
    ; TextIO.output(TextIO.stdOut, digistr)
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* string -> unit *)
(* prints the requirement *)
fun displayreq digi =
    ( TextIO.output(TextIO.stdOut, "The requirements are: \n")
    ; TextIO.output(TextIO.stdOut, digi)
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> unit *)
(* asks the user if he wants to end the program *)
fun quit () =
    ( TextIO.output(TextIO.stdOut, "Do you want to close digievotool?\n")
    ; TextIO.output(TextIO.stdOut, "Yes or No\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> bool *)
(* user inputs if he wants to leave or not *)
fun exitanswer () =
    case valOf (TextIO.inputLine(TextIO.stdIn)) of
        "Yes\n" => true
      | "yes\n"  => true
      | "no\n" => false
      | "No\n" => false
      | _ =>
        ( TextIO.output(TextIO.stdOut, "Please answer Yes or No.\n")
        ; exitanswer());


(* unit -> unit *)
(* main function of the whole tool *)
fun digievotool () =
    let
      fun A () : unit = ( prelude(); help(); whatlevel(); B() )

      and B () = let
        val (dg, num) = levelselector ()
        val dgstr = levelstring dg
      in
        ( whatdigimon ("Select a digimon (enter a number):\n", dgstr)
        ; C(num, levellist dg)
        )
      end

      and C (num, diglst) = let
        val dg = List.nth (diglst, pickdigimon (1, num)-1)
        val evol = ht.lookup evotable (strprefrem dg)
        val dgstr = evostring evol
      in
        ( whatdigimon ("Select an evolution (enter a number):\n", dgstr)
        ; D(num, evol))
      end

      and D (num, diglst) = let
        val dg = List.nth (diglst, pickdigimon(1, num)-1)
        val reql = ht.lookup reqtable (strprefrem dg)
        val reqstr = reqstring reql
      in
        (displayreq (reqstr)
        ; quit()
        ; E()
        )
      end

      and E () =
          if exitanswer() then ()
          else digievotool()
    in
      A ()
    end;

