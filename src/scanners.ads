package Scanners
  with SPARK_Mode => On
is

   Max_Input_Length : constant := 1024;
   type Small_Int is range 0 .. Max_Input_Length + 1;
   subtype Range_Lower is Small_Int range 1 .. Max_Input_Length + 1;
   subtype Range_Upper is Small_Int range 1 .. Max_Input_Length + 1;
   subtype Range_Size is Small_Int range 0 .. Max_Input_Length;

   type Scanner is private;
   type Lexeme is private;

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

   procedure Next (Self : in out Scanner)
   with
     Global => null,
     Post   =>
       Lexeme_Size (Self) = Lexeme_Size (Self'Old)
       or else Lexeme_Size (Self) = Lexeme_Size (Self'Old) + 1;

   function Width (Self : Lexeme) return Range_Size;

private

   type Scanner is record
      Input  : String (1 .. Max_Input_Length);
      Start  : Range_Lower := 1;
      Cursor : Range_Upper := 1;
      Length : Range_Size := 0;
   end record
   with Invariant => Start <= Cursor and then Cursor <= Length + 1;

   type Lexeme is record
      Lower : Range_Lower := 1;
      Upper : Range_Upper := 1;
   end record
   with Invariant => Lower <= Upper and then Upper - Lower <= Range_Size'Last;

   function Lexeme_Size (Self : Scanner) return Range_Size
   is (Self.Cursor - Self.Start);

   function Remaining (Self : Scanner) return Range_Size
   is (Self.Length - (Self.Cursor - 1));

   function Width (Self : Lexeme) return Range_Size
   is (Self.Upper - Self.Lower);

end Scanners;
