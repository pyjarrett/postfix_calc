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

         when others =>
            null;
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

      pragma Assert (Is_Running (Self));
      Pop (Self, Left);
      pragma Assert (Stack_Size (Self) >= 1);
      Pop (Self, Right);
      pragma Assert (Stack_Size (Self) + 2 <= Max_Stack_Size);
      pragma Assert (Is_Running (Self));
      if Left not in Bounded_Value or else Right not in Bounded_Value then
         Self.Status := Value_Out_Of_Bounds;
         return;
      end if;
      Push (Self, Left + Right);
      pragma Assert (Is_Running (Self));
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
         Self.Status := Value_Out_Of_Bounds;
         return;
      end if;
      Push (Self, Left + Right);
      pragma Assert (Is_Running (Self));
   end Op_Subtract;

end Machines;
