package body Scanners
  with SPARK_Mode => On
is

   procedure Load_Input (Self : in out Scanner; Input : String) is
   begin
      Self.Input (1 .. Input'Length) := Input;
      Self.Start := 1;
      Self.Cursor := 1;
      Self.Length := Input'Length;
      Self.State := (if Self.Length = 0 then Empty else Ready);
   end Load_Input;

   procedure Ignore_Lexeme (Self : in out Scanner) is
   begin
      Self.Start := Self.Cursor;
   end Ignore_Lexeme;

   procedure Take_Lexeme (Self : in out Scanner; Output : out Lexeme_Range) is
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
   begin
      Tk := (Kind => End_Of_Input, Lexeme => (Lower => 1, Upper => 1));

      declare
         Skipped_Whitespace : Boolean;
      begin
         Skip_Whitespace (Self, Skipped_Whitespace);
         if not Has_Next (Self) then
            pragma Assert (Skipped_Whitespace);
            pragma Assert (not ACH.Is_Space (Peek (Self)));
            return;
         end if;
      end;

      pragma Assert (not ACH.Is_Space (Peek (Self)));
      pragma Assert (Has_Next (Self));
      while Has_Next (Self) and then not ACH.Is_Space (Peek (Self)) loop
         pragma
           Loop_Invariant
             (Is_Valid (Self)
                and then Remaining_Characters (Self)
                         <= Remaining_Characters (Self'Loop_Entry));
         pragma Loop_Variant (Decreases => Remaining_Characters (Self));
         Next (Self);
      end loop;

      Self.State := (if Has_Next (Self) then Ready else Complete);
      Tk.Kind := Op;
      Take_Lexeme (Self, Tk.Lexeme);
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
                and then Is_Valid (Self)
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

      if Has_Next (Self) then
         pragma Assert (not ACH.Is_Space (Peek (Self)));
      end if;

      if Skipped_Whitespace then
         Self.Start := Self.Cursor;
      end if;

      Ignore_Lexeme (Self);

      Self.State := (if Has_Next (Self) then Ready else Complete);
   end Skip_Whitespace;

end Scanners;
