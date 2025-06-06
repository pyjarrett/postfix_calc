package body Scanners
  with SPARK_Mode => Off
is

   function Lexeme_Size (Self : Scanner) return Range_Size is
   begin
      return Self.Cursor - Self.Start;
   end Lexeme_Size;

   function Remaining (Self : Scanner) return Range_Size is
   begin
      return Self.Length - (Self.Cursor - 1);
   end Remaining;

   procedure Load_Input (Self : in out Scanner; Input : String) is
   begin
      Self.Input (1 .. Input'Length) := Input;
      Self.Start := 1;
      Self.Cursor := 1;
      Self.Length := Input'Length;
      pragma Assert (Self.Cursor = 1);
      pragma Assert (Self.Length = Input'Length);
      pragma Assert (Input'Length - (1 - 1) = Input'Length);
      pragma Assert (Self.Length - (Self.Cursor - 1) = Input'Length);
   end Load_Input;

   procedure Take_Lexeme (Self : in out Scanner; Output : in out Lexeme) is
   begin
      Output.Lower := Self.Start;
      Output.Upper := Self.Cursor;

      pragma Assert (Width (Output) = (Self.Cursor - Self.Start));

      Self.Start := Self.Cursor;

      pragma Assert (Lexeme_Size (Self) = 0);
   end Take_Lexeme;

   procedure Next (Self : in out Scanner) is
   begin
      if Self.Cursor <= Self.Length then
         Self.Cursor := Self.Cursor + 1;
      end if;
   end Next;

   function Width (Self : Lexeme) return Range_Size is
   begin
      return (Self.Upper - Self.Lower);
   end Width;

end Scanners;
