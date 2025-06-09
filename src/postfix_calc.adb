with Ada.Text_IO;

with Machines;
with Scanners;

procedure Postfix_Calc with SPARK_Mode => Off is
   M : Machines.Machine;
   S : Scanners.Scanner;
begin
   Ada.Text_IO.Put_Line (Machines.Status (M)'Image);

   loop
      declare
         Input : constant String := Ada.Text_IO.Get_Line;
         use type Scanners.Small_Int;
      begin
         exit when Input = "exit" or else Input = "quit";
         Scanners.Load_Input (S, Input);

         while Scanners.Has_More_Characters (S) loop
            declare
               Tokens : constant Scanners.Token_Array := Scanners.Tokenize (S);
            begin
               for Tk of Tokens loop
                  declare
                     Lexeme    : constant String := Scanners.Image (Tk, S);
                     New_Value : Machines.Bounded_Value;
                  begin
                     if Scanners.Is_Number (Lexeme) then
                        begin
                           New_Value := Machines.Bounded_Value'Value (Lexeme);
                           Machines.Push (M, New_Value);
                           Ada.Text_IO.Put_Line
                             ("Pushed value: " & New_Value'Image);
                        exception
                           when Constraint_Error =>
                              null;
                        end;
                     end if;
                  end;
                  Ada.Text_IO.Put_Line (Tk'Image);
               end loop;
            end;
         end loop;
      end;
   end loop;
end Postfix_Calc;
