!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with derived type allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - contains multiple calls with and without value attribute
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type inner
      integer, allocatable :: i1
   end type

   type, extends(inner) :: cinner
      integer, allocatable :: i2
   end type

   type base
      class(inner), allocatable :: i1
      class(inner), pointer :: i2
   end type

   integer :: g1

   interface
      subroutine foo2 ( a )
         import base
         type(base) :: a
      end subroutine
   end interface

   contains

   subroutine foo1 ( a )
      type(base), value  :: a

      print *, 'foo1:'
      select type ( g => a%i1 )
         type is ( inner )
            print *, g%i1
            g%i1=1
         type is ( cinner )
            print *, g%i1, g%i2
            g%i1=1
            g%i2=2
      end select

      select type ( g => a%i2 )
         type is ( inner )
            print *, g%i1
            g%i1=11
         type is ( cinner )
            print *, g%i1, g%i2
            g%i1=11
            g%i2=12
      end select

      call foo2 ( a )

      print *, 'end foo1:'
      select type ( g => a%i1 )
         type is ( inner )
            print *, g%i1
         type is ( cinner )
            print *, g%i1, g%i2
      end select

      select type ( g => a%i2 )
         type is ( inner )
            print *, g%i1
         type is ( cinner )
            print *, g%i1, g%i2
      end select

   end subroutine

end module

subroutine foo2 ( a )
   use m, only: base, inner, cinner
   type(base) :: a
   interface
      subroutine foo3 ( a )
         import base
         type(base), value :: a
      end subroutine
   end interface

   print *, 'foo2:'
   select type ( g => a%i1 )
      type is ( inner )
         print *, g%i1
         g%i1=21
      type is ( cinner )
         print *, g%i1, g%i2
         g%i1=21
         g%i2=22
   end select

   select type ( g => a%i2 )
      type is ( inner )
         print *, g%i1
         g%i1=31
      type is ( cinner )
         print *, g%i1, g%i2
         g%i1=31
         g%i2=32
   end select

   call foo3( a )

   print *, 'end foo2:'

   select type ( g => a%i1 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

   select type ( g => a%i2 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

end subroutine


subroutine foo3 ( a )
   use m, only: base, inner, cinner
   type(base), value :: a

   print *, 'foo3:'

   select type ( g => a%i1 )
      type is ( inner )
         print *, g%i1
         g%i1=41
      type is ( cinner )
         print *, g%i1, g%i2
         g%i1=41
         g%i2=42
   end select

   select type ( g => a%i2 )
      type is ( inner )
         print *, g%i1
         g%i1=51
      type is ( cinner )
         print *, g%i1, g%i2
         g%i1=51
         g%i2=52
   end select


   print *, 'end foo3:'

   select type ( g => a%i1 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

   select type ( g => a%i2 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

end subroutine

program valueAllocatableComponentMultCalls002
   use m

   type(base) :: b1
   class(base), allocatable :: b2

   integer, pointer :: i1
   class(inner), pointer :: in1

   allocate ( i1, source = 10 )
   allocate ( in1, source = cinner(5,i1) )

   b1 = base(inner(15), in1 )

   print *, 'start:'

   select type ( g => b1%i1 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

   select type ( g => b1%i2 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

   call foo1(b1)

   print *, 'end:'

   select type ( g => b1%i1 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

   select type ( g => b1%i2 )
      type is ( inner )
         print *, g%i1
      type is ( cinner )
         print *, g%i1, g%i2
   end select

end program
