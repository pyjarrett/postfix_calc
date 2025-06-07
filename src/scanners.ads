with Ada.Characters.Latin_1;

package Scanners
  with SPARK_Mode => On
is

   Max_Input_Length : constant := 1024;
   subtype Small_Int is Integer range 0 .. Max_Input_Length + 1;
   subtype Range_Lower is Small_Int range 1 .. Max_Input_Length + 1;
   subtype Range_Upper is Small_Int range 1 .. Max_Input_Length + 1;
   subtype Range_Index is Small_Int range 1 .. Max_Input_Length;
   subtype Range_Size is Small_Int range 0 .. Max_Input_Length;

   -- Lexeme --
   type Lexeme is private;
   function Width (Self : Lexeme) return Range_Size;

   -- Scanner --

   type Scanner is private;

   No_More_Characters : constant Character := Ada.Characters.Latin_1.NUL;

   function Is_Done (Self : Scanner) return Boolean;

   function Lexeme_Size (Self : Scanner) return Range_Size;

   function Remaining (Self : Scanner) return Range_Size;

   procedure Load_Input (Self : in out Scanner; Input : String)
   with
     Pre  => Input'Length <= Max_Input_Length,
     Post => Remaining (Self) = Input'Length;

   procedure Take_Lexeme (Self : in out Scanner; Output : out Lexeme)
   with
     Post =>
       Lexeme_Size (Self) = 0 and then Width (Output) = Lexeme_Size (Self'Old);

   function Has_Next (Self : Scanner) return Boolean
   with Global => null;

   function Peek (Self : Scanner) return Character
   with Global => null;

   procedure Next (Self : in out Scanner)
   with
     Global => null,
     Pre    => Has_Next (Self),
     Post   =>
       Lexeme_Size (Self) = Lexeme_Size (Self'Old) + 1
       and then Remaining (Self) < Remaining (Self'Old);

   -- Tokens --

   type Token_Kind is (Op, End_Of_Input);

   type Token is record
      Kind : Token_Kind;
   end record;

   procedure Next_Token (Self : in out Scanner; Tk : out Token)
   with
     Global         => null,
     Pre            => Has_Next (Self),
     Contract_Cases =>
       (Has_Next (self) => Remaining (Self) < Remaining (Self'Old),
        others          => Remaining (Self) = Remaining (Self'Old));

private

   -- Lexeme --

   type Lexeme is record
      Lower : Range_Lower := 1;
      Upper : Range_Upper := 1;
   end record
   with Invariant => Lower <= Upper and then Upper - Lower <= Range_Size'Last;

   function Width (Self : Lexeme) return Range_Size
   is (Self.Upper - Self.Lower);

   -- Scanner --

   type Scanner is record
      Input  : String (1 .. Max_Input_Length);
      Start  : Range_Lower := 1;
      Cursor : Range_Upper := 1;
      Length : Range_Size := 0;
   end record
   with Invariant => Start <= Cursor and then Cursor <= Length + 1;

   function Is_Done (Self : Scanner) return Boolean
   is (Self.Start = Self.Length + 1);

   function Lexeme_Size (Self : Scanner) return Range_Size
   is (Self.Cursor - Self.Start);

   function Remaining (Self : Scanner) return Range_Size
   is (Self.Length - (Self.Cursor - 1));

   function Has_Next (Self : Scanner) return Boolean
   is (Self.Cursor in Range_Index and then Self.Cursor <= Self.Length);

   function Peek (Self : Scanner) return Character
   is (if Has_Next (Self)
       then Self.Input (Range_Index (Self.Cursor))
       else No_More_Characters);

end Scanners;
