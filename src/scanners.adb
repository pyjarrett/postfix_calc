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
      pragma Assert (Contains (Self, Output));

      Self.Start := Self.Cursor;
   end Take_Lexeme;

   procedure Next (Self : in out Scanner)
   with
     Refined_Post =>
       Lexeme_Size (Self) = Lexeme_Size (Self'Old) + 1
       and then Remaining_Characters (Self) < Remaining_Characters (Self'Old)
       and then (for all X in Self.Input'Range
                 => Self.Input (X) = Self.Input'Old (X))
       and then Self.Start = Self.Start'Old
       and then Is_Valid (Self)
   is
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
            return;
         end if;
      end;

      pragma Assert (not ACH.Is_Space (Peek (Self)));
      pragma Assert (Has_Next (Self));
      pragma Assert (Self.Start = Self.Cursor);
      pragma
        Assert
          (for all X in Self.Start .. Self.Cursor
           => not ACH.Is_Space (Self.Input (X)));
      while Has_Next (Self) and then not ACH.Is_Space (Peek (Self)) loop
         pragma
           Loop_Invariant
             (Is_Valid (Self)
                and then Remaining_Characters (Self)
                         <= Remaining_Characters (Self'Loop_Entry)
                and then (not ACH.Is_Space (Self.Input (Self.Cursor)))
                and then (for all X in Self.Input'Range
                          => Self.Input (X) = Self.Input'Loop_Entry (X))
                and then Self.Start = Self.Start'Loop_Entry
                and then Self.Cursor in Self.Input'Range
                and then (for all X in Self.Start'Loop_Entry .. Self.Cursor
                          => not ACH.Is_Space (Self.Input (X))));
         pragma Loop_Variant (Decreases => Remaining_Characters (Self));
         Next (Self);
      end loop;

      pragma Assert (Self.Start in Self.Input'Range);
      pragma Assert (Self.Cursor <= Self.Input'Last + 1);
      pragma
        Assert
          ((for all X in Self.Start .. Self.Cursor - 1
            => not ACH.Is_Space (Self.Input (X))));

      Self.State := (if Has_Next (Self) then Ready else Complete);
      Tk.Kind := Word;
      Take_Lexeme (Self, Tk.Lexeme);

      pragma Assert (Contains (Self, Tk));
      pragma
        Assert
          ((for all X in Tk.Lexeme.Lower .. Tk.Lexeme.Upper - 1
            => not ACH.Is_Space (Self.Input (X))));
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

   function Image (Tk : Token; S : Scanner) return String is
   begin
      return
        (if Tk.Lexeme.Lower = Tk.Lexeme.Upper
         then ""
         else S.Input (Tk.Lexeme.Lower .. Tk.Lexeme.Upper - 1));
   end Image;

   function Tokenize (Self : in out Scanner) return Token_Array is
      Tokens     : Token_Array (1 .. 1024) := [others => <>];
      Next_Index : Positive := Tokens'First;
   begin
      pragma Assert (Tokens'Length <= Positive'Last - 1);
      while Has_Next (Self) and then Next_Index <= Tokens'Last loop
         pragma
           Loop_Invariant
             (Is_Valid (Self)
                and then Remaining_Characters (Self)
                         <= Remaining_Characters (Self'Loop_Entry)
                and then (for all X in 1 .. Next_Index - 1
                          => Tokens (X).Lexeme.Lower <= Tokens (X).Lexeme.Upper
                             and then Tokens (X).Lexeme.Upper
                                      - Tokens (X).Lexeme.Lower
                                      <= Range_Size'Last));
         pragma Loop_Variant (Decreases => Remaining_Characters (Self));
         Next_Token (Self, Tokens (Next_Index));
         Next_Index := Next_Index + 1;
      end loop;
      return Tokens (1 .. Next_Index - 1);
   end Tokenize;

   function Is_Number (Input : String) return Boolean is
      Found_Decimal : Boolean := False;
   begin
      if Input'Length = 0 then
         return False;
      end if;

      case Input (Input'First) is
         when '.' =>
            if Input'Length = 1 then
               return False;
            else
               Found_Decimal := True;
            end if;

         when '0' .. '9' =>
            null;

         when '-' =>
            if Input'Length = 1 then
               return False;
            end if;

         when others =>
            return False;
      end case;

      for X of Input (Input'First + 1 .. Input'Last) loop
         case X is
            when '.' =>
               if Found_Decimal then
                  return False;
               else
                  Found_Decimal := True;
               end if;

            when '0' .. '9' =>
               null;

            when others =>
               return False;
         end case;
      end loop;
      return True;
   end Is_Number;
end Scanners;
