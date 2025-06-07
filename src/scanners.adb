with Scanners;

with Ada.Characters.Handling;

package body Scanners
  with SPARK_Mode => On
is
   package ACH renames Ada.Characters.Handling;

   procedure Load_Input (Self : in out Scanner; Input : String) is
   begin
      Self.Input (1 .. Input'Length) := Input;
      Self.Start := 1;
      Self.Cursor := 1;
      Self.Length := Input'Length;
      Self.State := (if Self.Length = 0 then Empty else Ready);
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
         Self.State := (if Has_Next (Self) then Ready else Complete);
      end if;
   end Next;

   procedure Next_Token (Self : in out Scanner; Tk : out Token) is
      Skipped_Whitespace : Boolean;
   begin
      Tk := (Kind => Scanners.End_Of_Input);

      Skip_Whitespace (Self, Skipped_Whitespace);
      pragma Unreferenced (Skipped_Whitespace);

      while Has_Next (Self) loop
         pragma
           Loop_Invariant
             (Has_Next (Self)
                and then Remaining_Characters (Self) > 0
                and then Has_Valid_Cursor (Self)
                and then Has_Valid_State (Self));
         pragma Loop_Variant (Decreases => Remaining_Characters (Self));
         Next (Self);
      end loop;
      Self.State := (if Has_Next (Self) then Ready else Complete);
   end Next_Token;

   procedure Skip_Whitespace
     (Self : in out Scanner; Skipped_Whitespace : out Boolean) is
   begin
      Skipped_Whitespace := False;
      while Has_Next (Self) and then ACH.Is_Space (Peek (Self)) loop
         pragma
           Loop_Invariant
             (Has_Next (Self)
                and then Remaining_Characters (Self) > 0
                and then Has_Valid_Cursor (Self)
                and then Has_Valid_State (Self)
                and then ((not Skipped_Whitespace
                           and then Remaining_Characters (Self)
                                    = Remaining_Characters (Self'Loop_Entry))
                          or else (Skipped_Whitespace
                                   and then Remaining_Characters (Self)
                                            < Remaining_Characters
                                                (Self'Loop_Entry))));
         pragma Loop_Variant (Decreases => Remaining_Characters (Self));
         Next (Self);
         Skipped_Whitespace := True;
      end loop;

      if Skipped_Whitespace then
         Self.Start := Self.Cursor;
      end if;

      Self.State := (if Has_Next (Self) then Ready else Complete);
   end Skip_Whitespace;

end Scanners;
