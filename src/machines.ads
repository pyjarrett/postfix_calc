with Interfaces;

package Machines
  with SPARK_Mode => On, Always_Terminates
is
   type Machine_Status is
     (Ok, Stack_Overflow, Stack_Underflow, Value_Out_Of_Bounds);

   type Value is new Interfaces.IEEE_Float_64;
   subtype Bounded_Value is Value range -1.0e24 .. 1.0e24;

   Max_Stack_Size : constant := 1024;
   type Element_Count is new Integer range 0 .. Max_Stack_Size;

   type Machine_Op is
     (Add, Subtract, Multiply, Divide, Dupe, Print, Dump_Stack, Error, Reset);

   type Machine is private;

   function Status (Self : Machine) return Machine_Status
   with Global => null;

   function Is_Running (Self : Machine) return Boolean
   is (Status (Self) = Ok)
   with Global => null, Depends => (Is_Running'Result => Self);

   function Is_Stopped (Self : Machine) return Boolean
   is (Status (Self) /= Ok)
   with Global => null;

   function Stack_Size (Self : Machine) return Element_Count
   with Global => null;

   function Is_Stack_Empty (Self : Machine) return Boolean
   is (Stack_Size (Self) = 0)
   with Global => null;

   function Is_Stack_Full (Self : Machine) return Boolean
   is (Stack_Size (Self) = Max_Stack_Size)
   with Global => null;

   function Peek (Self : Machine) return Value
   with Global => null, Pre => not Is_Stack_Empty (Self);

   function Peek (Self : Machine; Depth : Element_Count) return Value
   with
     Global => null,
     Pre    => Stack_Size (Self) > Depth and then not Is_Stack_Empty (Self);

   procedure Push (Self : in out Machine; Element : Bounded_Value)
   with
     Global         => null,
     Depends        => (Self => +Element),
     Contract_Cases =>
       (Is_Stack_Full (Self) => Status (Self) = Stack_Overflow,
        others               =>
          not Is_Stack_Empty (Self)
          and then Stack_Size (Self) = Stack_Size (Self'Old) + 1
          and then Peek (Self) = Element
          and then (for all X in 0 .. Stack_Size (Self'Old) - 1
                    => Peek (Self, X + 1) = Peek (Self'Old, X)));

   procedure Pop (Self : in out Machine; Element : in out Value)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Is_Stack_Empty (Self) => Status (Self) = Stack_Underflow,
        others                =>
          Stack_Size (Self) = Stack_Size (Self'Old) - 1
          and then Peek (Self'Old) = Element
          and then Is_Running (Self) = Is_Running (Self'Old));

   procedure Pop (Self : in out Machine; Count : Element_Count)
   with
     Global         => null,
     Contract_Cases =>
       (Stack_Size (Self) >= Count =>
          Stack_Size (Self) = Stack_Size (Self'Old) - Count,
        Stack_Size (Self) < Count  =>
          Stack_Size (Self) = Stack_Size (Self'Old));

   procedure Execute (Self : in out Machine; Op : Machine_Op)
   with
     Contract_Cases =>
       (Is_Stopped (Self) and then Op /= Reset =>
          Is_Stopped (Self) and then Stack_Size (Self) = Stack_Size (Self'Old),
        Is_Stopped (Self) and then Op = Reset  =>
          Is_Running (Self) and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                                 => true);

   function To_Machine_Op (Input : String) return Machine_Op
   with Global => null;

private

   subtype Addend is Bounded_Value;
   subtype Minuend is Addend;
   subtype Subtrahend is Minuend;
   subtype Multiplier is Addend;
   subtype Multiplicand is Addend;
   subtype Dividend is Addend;
   subtype Prohibited_Divisor is Value range -1.0e-4 .. 1.0e-4;
   subtype Stack_Index is Element_Count range 1 .. Max_Stack_Size;

   type Machine_Stack is array (Stack_Index) of Bounded_Value;

   type Machine is record
      Status : Machine_Status := Ok;
      Stack  : Machine_Stack;
      Top    : Element_Count := 0;
   end record;

   function Status (Self : Machine) return Machine_Status
   is (Self.Status);

   function Stack_Size (Self : Machine) return Element_Count
   is (Self.Top);

   function Peek (Self : Machine) return Value
   is (Self.Stack (Stack_Index (Stack_Size (Self))));

   function Peek (Self : Machine; Depth : Element_Count) return Value
   is (Self.Stack (Stack_Index (Stack_Size (Self) - Depth)));

   procedure Op_Add (Self : in out Machine)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 0) + Peek (Self'Old, 1))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Subtract (Self : in out Machine)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 1) - Peek (Self'Old, 0))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Multiply (Self : in out Machine)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 1) * Peek (Self'Old, 0))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Divide (Self : in out Machine)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1
            and then Peek (Self) = Peek (Self'Old, 1) / Peek (Self'Old, 0))
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Dupe (Self : in out Machine)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Is_Stack_Full (Self)  =>
          Status (Self) = Stack_Overflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        Is_Stack_Empty (Self) =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          (Stack_Size (Self) = Stack_Size (Self'Old) + 1
           and then Peek (Self, 0) = Peek (Self'Old, 0)
           and then Peek (Self, 1) = Peek (Self'Old, 0)));

   procedure Op_Print (Self : in out Machine)
   with
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Is_Stack_Empty (Self) =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          (Is_Running (Self)
           and then Stack_Size (Self) = Stack_Size (Self'Old) - 1));

end Machines;
