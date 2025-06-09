with Ada.Text_IO;

package body Machines
  with SPARK_Mode => On
is

   procedure Push (Self : in out Machine; Element : Value) is
   begin
      if Self.Top = Max_Stack_Size then
         Self.Status := Stack_Overflow;
         return;
      end if;

      Self.Top := Self.Top + 1;
      pragma Assert (Self.Top > 0);
      pragma Assert (Self.Top in Stack_Index);
      pragma Assert (Self.Status = Ok);
      Self.Stack (Self.Top) := Element;
      pragma Assert (Self.Stack (Self.Top) = Element);
      pragma Assert (Self.Status = Ok);
   end Push;

   procedure Pop (Self : in out Machine; Element : in out Value) is
   begin
      pragma Assert (Is_Running (Self));

      if Stack_Size (Self) = 0 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Element := Peek (Self);
      Self.Top := Self.Top - 1;
   end Pop;

   procedure Execute (Self : in out Machine; Op : Machine_Op) is
   begin
      if Is_Stopped (Self) then
         if Op = Reset then
            Self.Status := Ok;
         end if;
         return;
      end if;

      case Op is
         when Add =>
            Op_Add (Self);

         when Subtract =>
            Op_Subtract (Self);

            --  when Multiply =>
            --     null;

            --  when Divide =>
            --     null;

            --  when Dupe =>
            --     null;

            --  when Print =>
            --     null;

         when Dump_Stack =>
            Ada.Text_IO.Put_Line ("Status: " & Self.Status'Image);
            for Index in reverse 1 .. Self.Top loop
               Ada.Text_IO.Put_Line (Self.Stack (Index)'Image);
            end loop;

         when others =>
            Ada.Text_IO.Put_Line ("Ignored: " & Op'Image);
      end case;
   end Execute;

   procedure Op_Add (Self : in out Machine) is
      Left  : Value := 0.0;
      Right : Value := 0.0;
   begin
      pragma Assert (Is_Running (Self));
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Pop (Self, Left);
      Pop (Self, Right);
      if Left not in Bounded_Value or else Right not in Bounded_Value then
         Push (Self, Right);
         Push (Self, Left);
         Self.Status := Value_Out_Of_Bounds;
         return;
      end if;
      Push (Self, Left + Right);
   end Op_Add;

   procedure Op_Subtract (Self : in out Machine) is
      Left  : Value := 0.0;
      Right : Value := 0.0;
   begin
      if Stack_Size (Self) < 2 then
         Self.Status := Stack_Underflow;
         return;
      end if;

      Pop (Self, Left);
      Pop (Self, Right);
      if Left not in Bounded_Value or else Right not in Bounded_Value then
         Push (Self, Right);
         Push (Self, Left);
         Self.Status := Value_Out_Of_Bounds;
         return;
      end if;
      Push (Self, Left + Right);
   end Op_Subtract;

   function To_Machine_Op (Input : String) return Machine_Op is
   begin
      if Input = "+" then
         return Add;
      elsif Input = "-" then
         return Subtract;
      elsif Input = "*" then
         return Multiply;
      elsif Input = "/" then
         return Divide;
      elsif Input = "." then
         return Print;
      elsif Input = "dup" then
         return Dupe;
      elsif Input = "dump" then
         return Dump_Stack;
      elsif Input = "reset" then
         return Reset;
      else
         return Error;
      end if;
   end To_Machine_Op;

end Machines;
