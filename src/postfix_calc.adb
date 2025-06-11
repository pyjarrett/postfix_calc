with Ada.Text_IO;

with Machines;
with Scanners;

procedure Postfix_Calc with SPARK_Mode => Off is
   M : Machines.Machine;
   S : Scanners.Scanner;
begin
   loop
      <<REPL_START>>
      declare
         Input : constant String := Ada.Text_IO.Get_Line;
      begin
         exit when Input = "exit" or else Input = "quit";

         if Input'Length > Scanners.Max_Input_Length then
            Ada.Text_IO.Put_Line
              ("Input line is too long:"
               & Input'Length'Image
               & ", max is"
               & Scanners.Max_Input_Length'Image);
            goto REPL_START;
         end if;

         Scanners.Load_Input (S, Input);
         while Scanners.Has_More_Characters (S) loop
            declare
               Tokens : constant Scanners.Token_Array := Scanners.Tokenize (S);
               use type Scanners.Token_Kind;
            begin
               for Tk of Tokens loop
                  if Tk.Kind = Scanners.Word then
                     declare
                        Lexeme    : constant String := Scanners.Image (Tk, S);
                        New_Value : Machines.Bounded_Value;
                        Op        : Machines.Machine_Op := Machines.Error;
                     begin
                        if Scanners.Is_Number (Lexeme) then
                           begin
                              New_Value :=
                                Machines.Bounded_Value'Value (Lexeme);
                              Machines.Push (M, New_Value);
                           exception
                              when Constraint_Error =>
                                 Ada.Text_IO.Put_Line
                                   ("Error in value:" & Lexeme);
                                 null;
                           end;
                        else
                           Op := Machines.To_Machine_Op (Lexeme);
                           Machines.Execute (M, Op);
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end;
   end loop;
end Postfix_Calc;
