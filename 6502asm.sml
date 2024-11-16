structure Table = HashTableFn(struct
 type hash_key = string
 val hashVal = HashString.hashString
 val sameKey = op=
end)

datatype modes = IMMEDIATE | ABSOLUTE     | XABSOLUTE   |
                 YABSOLUTE | ZEROPAGE     | XZERO       |
                 YZERO     | XINDIRECT    | YINDIRECT   |
                 RELATIVE  | ABSOINDIRECT | ACCUMULATOR |
                 IMPLIED

type opcode_size = (modes * int * int) list


(* Tables *)
val opTable : (opcode_size) Table.hash_table = Table.mkTable (128, Empty);
val symTable : (string) Table.hash_table = Table.mkTable (128, Empty);

(******* Filling the opcode table *************)

val _ = Table.insert opTable("LDA", [(IMMEDIATE, 0xA9,2), (ABSOLUTE, 0xAD, 3),
                                     (XABSOLUTE, 0xBD, 3), (YABSOLUTE, 0xB9,3),
                                     (ZEROPAGE, 0xA5, 2), (XZERO, 0xB5,2),
                                     (XINDIRECT, 0xA1, 2), (YINDIRECT, 0xB1, 2)]);
val _ = Table.insert opTable("LDX", [(IMMEDIATE, 0xA2, 2), (ABSOLUTE, 0xAE, 3),
                                     (YABSOLUTE, 0xBE, 3), (ZEROPAGE, 0xA6, 2),
                                     (YZERO, 0xB6, 2)]);
val _ = Table.insert opTable("LDY", [(IMMEDIATE, 0xA0, 2), (ABSOLUTE, 0xAC, 3),
                                     (XABSOLUTE, 0xBC, 3), (ZEROPAGE, 0xA4, 2),
                                     (XZERO, 0xB4, 2)]);
val _ = Table.insert opTable("STA", [(ABSOLUTE, 0x8D, 3), (XABSOLUTE, 0x9D, 3),
                                     (YABSOLUTE, 0x99, 3), (ZEROPAGE, 0x85, 2),
                                     (XZERO, 0x95, 2), (XINDIRECT, 0x81, 2),
                                     (YZERO, 0x91, 2)]);
val _ = Table.insert opTable("STX", [(ABSOLUTE, 0x8E, 3), (ZEROPAGE, 0x86, 2),
                                     (YZERO, 0x96, 2)]);
val _ = Table.insert opTable("STY", [(ABSOLUTE, 0x8C, 3), (ZEROPAGE, 0x84, 2),
                                     (XZERO, 0x94, 2)]);
val _ = Table.insert opTable("TAX", [(IMPLIED, 0xAA, 1)]);
val _ = Table.insert opTable("TAY", [(IMPLIED, 0xA8, 1)]);
val _ = Table.insert opTable("TSX", [(IMPLIED, 0xBA, 1)]);
val _ = Table.insert opTable("TXA", [(IMPLIED, 0x8A, 1)]);
val _ = Table.insert opTable("TXS", [(IMPLIED, 0x9A, 1)]);
val _ = Table.insert opTable("TYA", [(IMPLIED, 0x98, 1)]);
val _ = Table.insert opTable("PHA", [(IMPLIED, 0x48, 1)]);
val _ = Table.insert opTable("PHP", [(IMPLIED, 0x08, 1)]);
val _ = Table.insert opTable("PLA", [(IMPLIED, 0x68, 1)]);
val _ = Table.insert opTable("PLP", [(IMPLIED, 0x28, 1)]);
val _ = Table.insert opTable("ASL", [(ACCUMULATOR, 0x0A, 1), (ABSOLUTE, 0x0E, 3),
                                     (XABSOLUTE, 0x1E, 3), (ZEROPAGE, 0x06, 2),
                                     (XZERO, 0x16, 2)]);
val _ = Table.insert opTable("LSR", [(ACCUMULATOR, 0x4A, 1), (ABSOLUTE, 0x4E, 3),
                                     (XABSOLUTE, 0x5E, 3), (ZEROPAGE, 0x46, 2),
                                     (XZERO, 0x56, 2)]);
val _ = Table.insert opTable("ROL", [(ACCUMULATOR, 0x2A, 1), (ABSOLUTE, 0x2E, 3),
                                     (XABSOLUTE, 0x3E, 3), (ZEROPAGE, 0x26, 2),
                                     (XZERO, 0x36, 2)]);
val _ = Table.insert opTable("ROR", [(ACCUMULATOR, 0x6A, 1), (ABSOLUTE, 0x6E, 3),
                                     (XABSOLUTE, 0x7E, 3), (ZEROPAGE, 0x66, 2),
                                     (XZERO, 0x76, 2)]);
val _ = Table.insert opTable("AND", [(IMMEDIATE, 0x29, 2), (ABSOLUTE, 0x2D, 3),
                                     (XABSOLUTE, 0x3D, 3), (YABSOLUTE, 0x39, 3),
                                     (ZEROPAGE, 0x25, 2), (XZERO, 0x35, 2),
                                     (XINDIRECT, 0x21, 2), (YZERO, 0x31, 2)]);
val _ = Table.insert opTable("BIT", [(ABSOLUTE, 0x2C, 3), (ZEROPAGE, 0x24, 2)]);
val _ = Table.insert opTable("EOR", [(IMMEDIATE, 0x49, 2), (ABSOLUTE, 0x4D, 3),
                                     (XABSOLUTE, 0x5D, 3), (YABSOLUTE, 0x59, 3),
                                     (ZEROPAGE, 0x45, 2), (XZERO, 0x55, 2),
                                     (XINDIRECT, 0x41, 2), (YZERO, 0x51, 2)]);
val _ = Table.insert opTable("ORA", [(IMMEDIATE, 0x09, 2), (ABSOLUTE, 0x0D, 3),
                                     (XABSOLUTE, 0x1D, 3), (YABSOLUTE, 0x19, 3),
                                     (ZEROPAGE, 0x05, 2), (XZERO, 0x15, 2),
                                     (XINDIRECT, 0x01, 2), (YZERO, 0x11, 2)]);
val _ = Table.insert opTable("ADC", [(IMMEDIATE, 0x69, 2), (ABSOLUTE, 0x6D, 3),
                                     (XABSOLUTE, 0x7D, 3), (YABSOLUTE, 0x79, 3),
                                     (ZEROPAGE, 0x65, 2), (XZERO, 0x75, 2),
                                     (XINDIRECT, 0x61, 2), (YZERO, 0x71, 2)]);
val _ = Table.insert opTable("CMP", [(IMMEDIATE, 0xC9, 2), (ABSOLUTE, 0xCD, 3),
                                     (XABSOLUTE, 0xDD, 3), (YABSOLUTE, 0xD9, 3),
                                     (ZEROPAGE, 0xC5, 2), (XZERO, 0xD5, 2),
                                     (XINDIRECT, 0xC1, 2), (YZERO, 0xD1, 2)]);
val _ = Table.insert opTable("CPX", [(IMMEDIATE, 0xE0, 2), (ABSOLUTE, 0xEC, 3),
                                     (ZEROPAGE, 0xE4, 2)]);
val _ = Table.insert opTable("CPY", [(IMMEDIATE, 0xC0, 2), (ABSOLUTE, 0xCC, 3),
                                     (ZEROPAGE, 0xC4, 2)]);
val _ = Table.insert opTable("SBC", [(IMMEDIATE, 0xE9, 2), (ABSOLUTE, 0xED, 3),
                                     (XABSOLUTE, 0xFD, 3), (YABSOLUTE, 0xF9, 3),
                                     (ZEROPAGE, 0xE5, 2), (XZERO, 0xF5, 2),
                                     (XINDIRECT, 0xE1, 2), (YZERO, 0xF1, 2)]);
val _ = Table.insert opTable("DEC", [(ABSOLUTE, 0xCE, 3), (XABSOLUTE, 0xDE, 3),
                                     (ZEROPAGE, 0xC6, 2), (XZERO, 0xD6, 2)]);
val _ = Table.insert opTable("DEX", [(IMPLIED, 0xCA, 1)]);
val _ = Table.insert opTable("DEY", [(IMPLIED, 0x88, 1), (ABSOLUTE, 0xEE, 3),
                                     (XABSOLUTE, 0xFE, 3), (ZEROPAGE, 0xE6, 2),
                                     (XZERO, 0xF6, 2)]);
val _ = Table.insert opTable("INX", [(IMPLIED, 0xE8, 1)]);
val _ = Table.insert opTable("INY", [(IMPLIED, 0xC8, 1)]);
val _ = Table.insert opTable("BRK", [(IMPLIED, 0x00, 1)]);
val _ = Table.insert opTable("JMP", [(ABSOLUTE, 0x4C, 3), (ABSOINDIRECT, 0x6C, 3)]);
val _ = Table.insert opTable("JSR", [(ABSOLUTE, 0x20, 3)]);
val _ = Table.insert opTable("RTI", [(IMPLIED, 0x40, 1)]);
val _ = Table.insert opTable("RTS", [(IMPLIED, 0x60, 1)]);
val _ = Table.insert opTable("BCC", [(RELATIVE, 0x90, 2)]);
val _ = Table.insert opTable("BCS", [(RELATIVE, 0xB0, 2)]);;
val _ = Table.insert opTable("BEQ", [(RELATIVE, 0xF0, 2)]);
val _ = Table.insert opTable("BMI", [(RELATIVE, 0x30, 2)]);
val _ = Table.insert opTable("BNE", [(RELATIVE, 0xD0, 2)]);
val _ = Table.insert opTable("BPL", [(RELATIVE, 0x10, 2)]);
val _ = Table.insert opTable("BVC", [(RELATIVE, 0x50, 2)]);
val _ = Table.insert opTable("BVS", [(RELATIVE, 0x70, 2)]);
val _ = Table.insert opTable("CLC", [(IMPLIED, 0x18, 1)]);
val _ = Table.insert opTable("CLD", [(IMPLIED, 0xD8, 1)]);
val _ = Table.insert opTable("CLI", [(IMPLIED, 0x58, 1)]);
val _ = Table.insert opTable("CLV", [(IMPLIED, 0xB8, 1)]);
val _ = Table.insert opTable("SEC", [(IMPLIED, 0x38, 1)]);
val _ = Table.insert opTable("SED", [(IMPLIED, 0xF8, 1)]);
val _ = Table.insert opTable("SEI", [(IMPLIED, 0x78, 1)]);
val _ = Table.insert opTable("NOP", [(IMPLIED, 0xEA, 1)]);
(*********************************************************)

(* Deque for getting addressing mode on second pass *)
val deque = Fifo.empty;

(* Utility functions
 *)
val getLines = String.tokens Char.isCntrl;
val getInstructions = String.tokens Char.isSpace;
val noComma = String.tokens (fn c => c = #",");
val hasComma = ((List.exists (fn c => c = #",")) o explode);
val hasNoX = ((List.exists (fn c => c = #"X")) o explode);
val hasParen = ((List.exists (fn c => c = #"(")) o explode);
val noParen = (implode o (List.filter (fn c => c <> #"(" andalso c <> #")")) o explode);
val hasComments = ((List.exists (fn c => c = #";")) o explode);
val noComments = String.tokens (fn c => c = #";");
val pos = String.sub;
val hex = explode "0123456789ABCDEF";
val int2byte = Word8.fromInt;
val branches = ["BCC", "BCS", "BEQ", "BMI",
                "BNE", "BPL", "BVC", "BVS"];

fun firstByte addr = String.substring (addr, 0, 2);
fun secondByte addr = String.substring (addr, 2, 2);

fun removeComments code =
    let
      fun noCo [] = []
        | noCo (x::xs) =
          if hasComments x
          then let
            val [first,second] = noComments x
          in
            first :: (noCo xs)
          end
          else x :: (noCo xs)
    in
      noCo (getLines code)
    end;

fun hexInt #"A" = 10
  | hexInt #"B" = 11
  | hexInt #"C" = 12
  | hexInt #"D" = 13
  | hexInt #"E" = 14
  | hexInt #"F" = 15
  | hexInt  _  = raise Fail "Not hex";

fun s2i addr =
    case Int.fromString (str addr) of
        SOME(X) => X
      | NONE    => hexInt addr;

fun hexString2Int addr =
    let
      val [first, second] = explode addr
    in
      16 * (s2i first) + (s2i second)
    end;

fun hexString2Byte addr =
    int2byte (hexString2Int addr);

fun int2HS number =
    let
      val q = Int.quot (number,16)
      val r = Int.rem  (number,16)
      val hex = implode hex
    in
      str (pos(hex, q)) ^ str (pos(hex, r))
    end;

fun mode2String ABSOINDIRECT = "ABSOINDIRECT"
  | mode2String ABSOLUTE     = "ABSOLUTE"
  | mode2String ACCUMULATOR  = "ACCUMULATOR"
  | mode2String IMMEDIATE    = "IMMEDIATE"
  | mode2String IMPLIED      = "IMPLIED"
  | mode2String RELATIVE     = "RELATIVE"
  | mode2String XABSOLUTE    = "XABSOLUTE"
  | mode2String XINDIRECT    = "XINDIRECT"
  | mode2String XZERO        = "XZERO"
  | mode2String YABSOLUTE    = "YABSOLUTE"
  | mode2String YINDIRECT    = "YINDIRECT"
  | mode2String YZERO        = "YZERO"
  | mode2String ZEROPAGE     = "ZEROPAGE";



fun saveBin [] outstream = BinIO.closeOut outstream
  | saveBin (x::xs) outstream =
    let
      fun writeBytes         [] stream = ()
        | writeBytes (byte::bs) stream =
          ( BinIO.output1 (stream, hexString2Byte byte)
          ; writeBytes bs stream);
    in
      ( writeBytes (getInstructions x) outstream
      ; saveBin xs outstream)
    end;

fun saveFile      []      [] deq outstream = TextIO.closeOut outstream
  | saveFile (x::xs) (y::ys) deq outstream =
    let
      fun memoryBytes bytes =
          if bytes <= 255
          then "00" ^ (int2HS bytes)
          else int2HS bytes;

      fun indexPrint instr =
          let
            val instr = getInstructions instr
            val he = hd instr
            val re = (concat o List.map (fn s => s ^ " ")) (tl instr)
            val re = String.substring(re, 0, size re-1)
          in
            he ^ "\t" ^ re
          end;

      fun writeText text stream = TextIO.output (stream, text ^ "\n")
    in
      let
        val (deq', bytes: {1: modes, 2: int}) = Fifo.dequeue deq
      in
        if x = ""
        then ( writeText ("\t\t\t\t\t" ^ y) outstream
             ; saveFile xs ys deq outstream)
        else if size x <= 5
        then
          ( writeText (memoryBytes (#2 bytes) ^ "\t\t" ^ x ^ "\t\t\t" ^ y) outstream
          ; saveFile xs ys deq' outstream)
        else if length ((getInstructions o concat o removeComments)   y) <= 2
        then
          ( writeText (memoryBytes (#2 bytes) ^ "\t\t" ^ x ^ "\t\t" ^ y) outstream
          ; saveFile xs ys deq' outstream)
        else
          ( writeText (memoryBytes (#2 bytes) ^ "\t\t" ^ x ^ "\t" ^ (indexPrint y)) outstream
          ; saveFile xs ys deq' outstream)
      end
    end;

fun lookup table elem =
    SOME (Table.lookup table elem)
    handle Empty => NONE;


fun firstPass code =
    let
      fun insertSymbol sym addr =
          Table.insert symTable(sym, addr);

      fun isMember c [] = false
        | isMember c (x::xs) =
          if c = x
          then true
          else isMember c xs;

      fun numDigits addr =
          length (List.filter (fn c => isMember c hex) (explode addr));

      fun indirectXY first second addr =
          if pos(addr, size addr - 1) = #")"
          then "(" ^ first ^ "," ^ second ^ ")"
          else "(" ^ first ^ ")," ^ second;

      fun modeFromSymbol addr =
          if hasComma addr
          then if hasParen addr
               then let
                 val [first, second] = (noComma o noParen) addr
               in
                 case lookup symTable first of
                     SOME (X) => indirectXY X second addr
                   | NONE => raise Fail ("Symbol doesn't exist" ^ addr)
               end
               else let
                 val [first, second] = noComma addr
               in
                 case lookup symTable first of
                     SOME (X) => X ^ "," ^ second
                   | NONE => raise Fail ("Symbol doesn't exist" ^ addr)
               end
          else
            case lookup symTable addr of
                SOME (X) => X
              | NONE => raise Fail ("Symbol doesn't exist" ^ addr);


      fun getAddressingMode addr =
          let
            val first = pos(addr, 0)
            val last = pos(addr, size addr - 1)
          in
            case first of
                #"#" => IMMEDIATE
              | #"$" => if numDigits addr >= 4
                        then (case last of
                                  #"X" => XABSOLUTE
                                | #"Y" => YABSOLUTE
                                | _    => ABSOLUTE)
                        else if last = #"X"
                        then XZERO
                        else if last = #"Y"
                        then YZERO
                        else ZEROPAGE
              | #"(" => if last = #")" andalso hasNoX addr
                        then ABSOINDIRECT
                        else if last = #")"
                        then XINDIRECT
                        else YINDIRECT
              | _ => getAddressingMode (modeFromSymbol addr)
          end;


      fun getBytes  ([] : opcode_size) mode = raise Fail "Mode doesn't exist"
        | getBytes (x::xs) mode =
          if #1 x = mode
          then #3 x
          else getBytes xs mode;

      fun updateDequeBytes instr addr bytes deq =
          case lookup opTable instr of
              SOME (X) => ( if length X = 1
                            then let
                              val opcode = hd X
                            in
                              (bytes + (#3 opcode),
                               Fifo.enqueue (deq, (#1 opcode, bytes)))
                            end
                            else let
                              val mode = getAddressingMode addr
                            in
                              (bytes + (getBytes X mode),
                               Fifo.enqueue (deq, (mode, bytes)))
                            end )
            | NONE => raise Fail "Instruction doesn't exist";

      fun updateTableDequeBytes instr addr bytes deq sym =
          let
            val byteDeq = updateDequeBytes instr addr bytes deq
          in
            (insertSymbol sym ("$" ^ Int.toString bytes)
            ; byteDeq)
          end;

      fun readLine1stPass [sym, instr, addr] bytes deq =
          if instr = "EQ"
          then ( insertSymbol sym addr
               ; (bytes, deq))
          else
            updateTableDequeBytes instr addr bytes deq sym
        | readLine1stPass [instr, addr] bytes deq =
          updateDequeBytes instr addr bytes deq
        | readLine1stPass [instr] bytes deq =
          case lookup opTable instr of
              SOME(X) => let val opcode = hd X
                         in
                           (bytes + (#3 opcode),
                            Fifo.enqueue (deq, (#1 opcode, bytes)))
                         end
            | NONE => raise Fail "Should have a valid instruction here";
    fun firstPass' [] bytes deq = (bytes, deq)
      | firstPass' (x::xs) bytes deq=
        let
          val line = getInstructions x
          val (bytes', deq') = readLine1stPass line bytes deq
        in
          firstPass' xs bytes' deq'
        end
    in
      firstPass' code 0 deque
    end;
fun secondPass code deque =
    let

      fun getBytesFromAddr ABSOINDIRECT addr = String.substring(addr,2,4)
        | getBytesFromAddr ABSOLUTE     addr = String.substring(addr,1,4)
        | getBytesFromAddr IMMEDIATE    addr = String.substring(addr,2,2)
        | getBytesFromAddr RELATIVE     addr = String.substring(addr,1,4)
        | getBytesFromAddr XABSOLUTE    addr = String.substring(addr,1,4)
        | getBytesFromAddr XINDIRECT    addr = String.substring(addr,2,2)
        | getBytesFromAddr XZERO        addr = String.substring(addr,1,2)
        | getBytesFromAddr YABSOLUTE    addr = String.substring(addr,1,4)
        | getBytesFromAddr YINDIRECT    addr = String.substring(addr,2,2)
        | getBytesFromAddr YZERO        addr = String.substring(addr,1,2)
        | getBytesFromAddr ZEROPAGE     addr = String.substring(addr,1,2)
        | getBytesFromAddr _ addr = raise Fail "Address/Symbol wrong";

      fun FSByte addr =
          let
            val addr = String.substring(addr, 1, size addr-1)
          in
            if size addr = 4
            then (secondByte addr) ^ " " ^ (firstByte addr)
            else if size addr = 1
            then "0" ^ addr
            else addr
          end;

      fun validAddr addr =
          let
            val first = pos(addr,0)
          in
            if  first = #"(" orelse first = #"$"
                orelse first = #"#"
            then true
            else false
          end;

      fun getAddr addr (mode : {1: modes, 2: int}) =
          if hasComma addr
          then if hasParen addr
               then let
                 val [first, second] = (noComma o noParen) addr
               in
                 (case lookup symTable first of
                      SOME (X) => FSByte X
                    | NONE => if validAddr addr
                              then FSByte (getBytesFromAddr (#1 mode)  addr)
                              else raise Fail ("Invalid address " ^ addr))
               end
               else let
                 val [first, second] = noComma addr
               in
                 (case lookup symTable first of
                      SOME (X) => FSByte X
                    | NONE => if validAddr addr
                              then FSByte (getBytesFromAddr (#1 mode)  addr)
                              else raise Fail ("Invalid address " ^ addr))
               end
          else
            (case lookup symTable addr of
                 SOME (X) => FSByte X
               | NONE => if validAddr addr
                         then FSByte (getBytesFromAddr (#1 mode) addr)
                         else raise Fail ("Invalid address " ^ addr));

      fun getInstByte ([] : opcode_size) (mode : {1: modes, 2: int}) = raise Fail ("No such mode " ^ mode2String (#1 mode))
        | getInstByte (x::xs) mode =
          if (#1 x) = (#1 mode)
          then int2HS (#2 x)
          else getInstByte xs mode;

      fun isBranch instr =
          List.exists (fn c => c = instr) branches;


      fun branchAddr addr (bytes : {1: modes, 2: int})  =
          let
            val addr' = hexString2Int addr
            val bytes = #2 bytes + 2
          in
            if bytes > addr'
            then int2HS (255 - (bytes - addr') + 1)
            else int2HS (addr' + bytes)
          end;

      fun readLine2ndPass [sym, instr, addr] deq =
          if instr = "EQ"
          then ("", deq)
          else let
            val (deq', mode_bytes) = Fifo.dequeue deq
            val opcode = getInstByte (Table.lookup opTable instr) mode_bytes
            val addr' = getAddr addr mode_bytes
          in
            (opcode ^ " " ^ addr', deq')
          end
        | readLine2ndPass [instr, addr] deq =
          let
            val (deq', mode_bytes) = Fifo.dequeue deq
            val opcode = getInstByte (Table.lookup opTable instr) mode_bytes
            val addr' = getAddr addr mode_bytes
          in
            if isBranch instr
            then (opcode ^ " " ^ (branchAddr  addr' mode_bytes), deq')
            else (opcode ^ " " ^ addr', deq')
          end
        | readLine2ndPass [instr] deq =
          let
            val (deq', mode_bytes) = Fifo.dequeue deq
            val opcode = getInstByte (Table.lookup opTable instr) mode_bytes
          in
            (opcode, deq')
          end

      fun secondPass'      [] deq = []
        | secondPass' (x::xs) deq =
          let
            val line = getInstructions x
            val (opcode, deq') = readLine2ndPass line deq
          in
            opcode :: (secondPass' xs deq')
          end
    in
      secondPass' code deque
    end;

fun assembler code programName =
    let
      val code' = removeComments code
      val outStream = BinIO.openOut programName
      val (_, deque') = firstPass code'
      val code'' = secondPass code' deque'
    in
      saveBin code'' outStream
    end;

fun assemblerList code fileName =
    let
      val code' = removeComments code
      val outStream = TextIO.openOut fileName
      val (_, deque') = firstPass code'
      val code'' = secondPass code' deque'
    in
      (TextIO.output (outStream, "MEMORY\t\tINSTRUCTION\t\tASSEMBLY\n\n")
      ; saveFile code'' (getLines code) deque' outStream)
    end;

val code = "MOVLEN EQ $00\nLOC1 EQ $0200\nLOC2 EQ $0300\nLDX MOVLEN ;LOAD LENGTH OF MOVE TO INDEX\nLOOP LDA LOC1,X ;LOAD BYTE TO BE MOVED\nSTA LOC2,X ;STORE BYTE TO BE MOVED\nDEX ;COUNT DOWN\nBPL LOOP ;IF NOT DONE, MOVE NEXT BYTE\nRTS ;DONE";

assembler code "program.bin";
assemblerList code "program.txt";
