with Ada.Characters.Latin_1;
with Ada.Characters.Handling;

package Scanners
  with SPARK_Mode => On, Pure, Always_Terminates
is
   package ACH renames Ada.Characters.Handling;

   Max_Input_Length : constant := 1024;
   subtype Small_Int is Integer range 0 .. Max_Input_Length + 1;
   subtype Range_Lower is Small_Int range 1 .. Max_Input_Length + 1;
   subtype Range_Upper is Small_Int range 1 .. Max_Input_Length + 1;
   subtype Range_Index is Small_Int range 1 .. Max_Input_Length;
   subtype Range_Size is Small_Int range 0 .. Max_Input_Length;

   -- Lexeme --
   type Lexeme_Range is private;
   function Width (Self : Lexeme_Range) return Range_Size;

   -- Scanner --

   type Scanner is private;

   No_More_Characters : constant Character := Ada.Characters.Latin_1.NUL;

   function Has_Input (Self : Scanner) return Boolean
   with Global => null;

   function Has_More_Characters (Self : Scanner) return Boolean
   with Global => null;

   function Has_Lexeme (Self : Scanner) return Boolean
   with Global => null;

   procedure Ignore_Lexeme (Self : in out Scanner)
   with
     Global => null,
     Post   =>
       Lexeme_Size (Self) = 0
       and then Remaining_Characters (Self) = Remaining_Characters (Self'Old)
       and then Has_Next (Self) = Has_Next (Self'Old)
       and then Peek (Self) = Peek (Self'Old);

   function Lexeme_Size (Self : Scanner) return Range_Size
   with Global => null, Post => Lexeme_Size'Result <= Input_Size (Self);

   function Remaining_Characters (Self : Scanner) return Range_Size
   with Global => null;

   function Input_Size (Self : Scanner) return Range_Size;

   procedure Load_Input (Self : in out Scanner; Input : String)
   with
     Global         => null,
     Depends        => (Self => +Input),
     Pre            => Input'Length <= Max_Input_Length,
     Contract_Cases =>
       (Input'Length = 0 => true,
        others           =>
          Remaining_Characters (Self) = Input'Length
          and then Has_More_Characters (Self)
          and then Input_Size (Self) = Input'Length);

   function Has_Next (Self : Scanner) return Boolean
   with
     Global => null,
     Post   =>
       ((Has_Next'Result and then Remaining_Characters (Self) > 0)
        or else (not Has_Next'Result and then Remaining_Characters (Self) = 0)
        or else (not Has_Next'Result and then not Has_Input (Self)));

   function Peek (Self : Scanner) return Character
   with Global => null;

   procedure Next (Self : in out Scanner)
   with
     Global => null,
     Pre    => Has_Next (Self),
     Post   =>
       Lexeme_Size (Self) = Lexeme_Size (Self'Old) + 1
       and then Remaining_Characters (Self) < Remaining_Characters (Self'Old);

   -- Tokens --

   type Token_Kind is (Word, End_Of_Input);

   type Token is record
      Kind   : Token_Kind := End_Of_Input;
      Lexeme : Lexeme_Range;
   end record;

   Max_Tokens : constant := 512;
   type Token_Array is array (Positive range <>) of Token;

   procedure Next_Token (Self : in out Scanner; Tk : out Token)
   with
     Global => null,
     Pre    => Has_Next (Self) and then Has_More_Characters (Self),
     Post   =>
       (Remaining_Characters (Self) < Remaining_Characters (Self'Old))
       and then (Tk.Kind = End_Of_Input
                 or else (Tk.Kind = Word
                          and then Width (Tk.Lexeme) > 0
                          and then Is_Word (Self, Tk)));

   function Image (Tk : Token; S : Scanner) return String;

   function Contains (Self : Scanner; Tk : Token) return Boolean;
   function Contains (Self : Scanner; Lexeme : Lexeme_Range) return Boolean;
   function Is_Word (Self : Scanner; Tk : Token) return Boolean;

   function Tokenize (Self : in out Scanner) return Token_Array
   with
     Global => null,
     Side_Effects,
     Pre    => Has_More_Characters (Self),
     Post   =>
       (if Has_Next (Self'Old)
        then Remaining_Characters (Self) < Remaining_Characters (Self'Old));

private

   -- Lexeme --

   type Lexeme_Range is record
      Lower : Range_Index := 1;
      Upper : Range_Upper := 1;
   end record
   with Invariant => Lower <= Upper and then Upper - Lower <= Range_Size'Last;

   function Width (Self : Lexeme_Range) return Range_Size
   is (Self.Upper - Self.Lower);

   -- Scanner --

   type Scanner_State is (Empty, Ready, Complete);

   function Is_Valid (Self : Scanner) return Boolean;

   type Scanner is record
      Input  : String (1 .. Max_Input_Length);
      Start  : Range_Lower := 1;
      Cursor : Range_Upper := 1;
      Length : Range_Size := 0;
      State  : Scanner_State := Empty;
   end record
   with Invariant => Is_Valid (Scanner);

   function Has_Valid_Cursor (Self : Scanner) return Boolean
   is (Self.Start <= Self.Cursor and then Self.Cursor <= Self.Length + 1);

   function Has_Valid_State (Self : Scanner) return Boolean
   is (case Self.State is
         when Empty =>
           Self.Length = 0 and then Self.Start = 1 and then Self.Cursor = 1,
         when Complete => Self.Cursor = Self.Length + 1,
         when others =>
           Self.Start <= Self.Cursor and then Self.Cursor <= Self.Length + 1);

   function Has_Input (Self : Scanner) return Boolean
   is (Self.Length > 0);

   function Has_More_Characters (Self : Scanner) return Boolean
   is (Self.Cursor <= Self.Length);

   function Has_Lexeme (Self : Scanner) return Boolean
   is (Self.Start <= Self.Length and then Self.Start < Self.Cursor);

   function Lexeme_Size (Self : Scanner) return Range_Size
   is (Self.Cursor - Self.Start);

   function Remaining_Characters (Self : Scanner) return Range_Size
   is (Self.Length - (Self.Cursor - 1));

   function Input_Size (Self : Scanner) return Range_Size
   is (Self.Length);

   function Has_Next (Self : Scanner) return Boolean
   is (Self.Cursor in Range_Index and then Has_More_Characters (Self));

   function Peek (Self : Scanner) return Character
   is (if Has_Next (Self)
       then Self.Input (Range_Index (Self.Cursor))
       else No_More_Characters);

   function Is_Valid (Self : Scanner) return Boolean
   is (Has_Valid_Cursor (Self) and then Has_Valid_State (Self));

   procedure Skip_Whitespace
     (Self : in out Scanner; Skipped_Whitespace : out Boolean)
   with
     Global => null,
     Pre    =>
       Is_Valid (Self)
       and then Has_Next (Self)
       and then Has_More_Characters (Self),
     Post   =>
       Is_Valid (Self)
       and then (Self.State = (if Has_Next (Self) then Ready else Complete))
       and then (if Has_Next (Self) then not ACH.Is_Space (Peek (Self)))
       and then (if not Has_Next (Self)
                 then
                   Skipped_Whitespace and then not ACH.Is_Space (Peek (Self)))
       and then (if Skipped_Whitespace
                 then
                   Remaining_Characters (Self)
                   < Remaining_Characters (Self'Old))
       and then ((Skipped_Whitespace
                  and then Remaining_Characters (Self)
                           < Remaining_Characters (Self'Old))
                 or else (not Skipped_Whitespace))
       and then (if not Skipped_Whitespace
                 then
                   Remaining_Characters (Self)
                   = Remaining_Characters (Self'Old))
       and then (Lexeme_Size (Self) = 0);

   function Contains (Self : Scanner; Tk : Token) return Boolean
   is (Contains (Self, Tk.Lexeme));

   function Contains (Self : Scanner; Lexeme : Lexeme_Range) return Boolean
   is (Lexeme.Upper <= Self.Length + 1);

   procedure Take_Lexeme (Self : in out Scanner; Output : out Lexeme_Range)
   with
     Pre  => Is_Valid (Self) and then Has_Lexeme (Self),
     Post =>
       Is_Valid (Self)
       and then Lexeme_Size (Self) = 0
       and then Width (Output) = Lexeme_Size (Self'Old)
       and then Remaining_Characters (Self) = Remaining_Characters (Self'Old)
       and then Has_Next (Self) = Has_Next (Self'Old)
       and then Peek (Self) = Peek (Self'Old)
       and then Contains (Self, Output)
       and then (if (for all X in Self.Start'Old .. Self.Cursor'Old - 1
                     => not ACH.Is_Space (Self.Input'Old (X)))
                 then
                   (for all X in Output.Lower .. Output.Upper - 1
                    => not ACH.Is_Space (Self.Input (X))));

   function Is_Word (Self : Scanner; Tk : Token) return Boolean
   is ((for all X in Tk.Lexeme.Lower .. Tk.Lexeme.Upper - 1
        => not ACH.Is_Space (Self.Input (X))));

end Scanners;
