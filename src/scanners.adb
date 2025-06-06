with Scanners;

package body Scanners
  with SPARK_Mode => On
is

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

   procedure Take_Lexeme (Self : in out Scanner; Output : out Lexeme) is
   begin
      Output.Lower := Self.Start;
      Output.Upper := Self.Cursor;

      pragma Assert (Width (Output) = (Self.Cursor - Self.Start));

      Self.Start := Self.Cursor;
   end Take_Lexeme;

   procedure Next (Self : in out Scanner) is
   begin
      if Has_Next (Self) then
         Self.Cursor := Self.Cursor + 1;
      end if;
   end Next;

   procedure Next_Token (Self : in out Scanner; Tk : out Token) is
   begin
      Tk := (Kind => Scanners.End_Of_Input);
      Next (Self);
   end Next_Token;

end Scanners;
