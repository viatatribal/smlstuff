(* A very simple spellchecker in SML based on
 * https://bernhardwenzel.com/articles/clojure-spellchecker/
 * It works, but calculating the levenshtein distance without
 * memoization is VERY slow if applied to a big file *)

fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;

fun convert word =
    String.tokens Char.isCntrl word;

val words = (convert o getContent) "wordsEn.txt";
fun isCorrect word =
    List.exists
        (fn st => st = word)
        words;

(* Levenshtein distance
 * It's extremely slow for the use in this code
 * but works perfectly if comparing two words *)
fun levDistance word1 word2 =
    let
      fun distance (i,j) =
          case (i,j) of
              (i,0) => i
            | (0,j) => j
            | (i,j) =>
              if String.sub(word1,i-1) = String.sub(word2,j-1)
              then distance (i-1,j-1)
              else let
                val dist1 = distance (i-1,j)
                val dist2 = distance (i,j-1)
                val dist3 = distance (i-1,j-1)
              in
                1 + Int.min(dist1, Int.min(dist2, dist3))
              end
    in
      distance (size word1, size word2)
    end;

fun minDistance word =
    List.foldl
        (fn (x,y) => let val z = levDistance word x
                     in if z = Int.min(z, #1 y)
                        then (z,x)
                        else y
                     end)
        (valOf Int.maxInt, "")
        words;

fun main word =
    if isCorrect word
    then print "Correct\n"
    else
      let
        val (_,w) =minDistance word
      in
        print ("Did you mean " ^ w ^ "?\n" )
      end;
