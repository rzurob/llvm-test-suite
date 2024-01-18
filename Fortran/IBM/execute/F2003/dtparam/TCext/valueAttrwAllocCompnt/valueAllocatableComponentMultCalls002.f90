! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentMultCalls002.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
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

   type inner(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i1
   end type

   type, extends(inner) :: cinner    ! (4)
      integer(k1), allocatable :: i2
   end type

   type base(k2)    ! (4)
      integer, kind                 :: k2
      class(inner(k2)), allocatable :: i1
      class(inner(k2)), pointer     :: i2
   end type

   integer :: g1

   interface
      subroutine foo2 ( a )
         import base
         type(base(4)) :: a
      end subroutine
   end interface

   contains

   subroutine foo1 ( a )
      type(base(4)), value  :: a

      print *, 'foo1:'
      select type ( g => a%i1 )
         type is ( inner(4) )
            print *, g%i1
            g%i1=1
         type is ( cinner(4) )
            print *, g%i1, g%i2
            g%i1=1
            g%i2=2
      end select

      select type ( g => a%i2 )
         type is ( inner(4) )
            print *, g%i1
            g%i1=11
         type is ( cinner(4) )
            print *, g%i1, g%i2
            g%i1=11
            g%i2=12
      end select

      call foo2 ( a )

      print *, 'end foo1:'
      select type ( g => a%i1 )
         type is ( inner(4) )
            print *, g%i1
         type is ( cinner(4) )
            print *, g%i1, g%i2
      end select

      select type ( g => a%i2 )
         type is ( inner(4) )
            print *, g%i1
         type is ( cinner(4) )
            print *, g%i1, g%i2
      end select

   end subroutine

end module

subroutine foo2 ( a )
   use m, only: base, inner, cinner
   type(base(4)) :: a
   interface
      subroutine foo3 ( a )
         import base
         type(base(4)), value :: a
      end subroutine
   end interface

   print *, 'foo2:'
   select type ( g => a%i1 )
      type is ( inner(4) )
         print *, g%i1
         g%i1=21
      type is ( cinner(4) )
         print *, g%i1, g%i2
         g%i1=21
         g%i2=22
   end select

   select type ( g => a%i2 )
      type is ( inner(4) )
         print *, g%i1
         g%i1=31
      type is ( cinner(4) )
         print *, g%i1, g%i2
         g%i1=31
         g%i2=32
   end select

   call foo3( a )

   print *, 'end foo2:'

   select type ( g => a%i1 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

   select type ( g => a%i2 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

end subroutine


subroutine foo3 ( a )
   use m, only: base, inner, cinner
   type(base(4)), value :: a

   print *, 'foo3:'

   select type ( g => a%i1 )
      type is ( inner(4) )
         print *, g%i1
         g%i1=41
      type is ( cinner(4) )
         print *, g%i1, g%i2
         g%i1=41
         g%i2=42
   end select

   select type ( g => a%i2 )
      type is ( inner(4) )
         print *, g%i1
         g%i1=51
      type is ( cinner(4) )
         print *, g%i1, g%i2
         g%i1=51
         g%i2=52
   end select


   print *, 'end foo3:'

   select type ( g => a%i1 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

   select type ( g => a%i2 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

end subroutine

program valueAllocatableComponentMultCalls002
   use m

   type(base(4)) :: b1
   class(base(4)), allocatable :: b2

   integer, pointer :: i1
   class(inner(4)), pointer :: in1

   allocate ( i1, source = 10 )
   allocate ( in1, source = cinner(4)(5,i1) )

   b1 = base(4)(inner(4)(15), in1 )

   print *, 'start:'

   select type ( g => b1%i1 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

   select type ( g => b1%i2 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

   call foo1(b1)

   print *, 'end:'

   select type ( g => b1%i1 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

   select type ( g => b1%i2 )
      type is ( inner(4) )
         print *, g%i1
      type is ( cinner(4) )
         print *, g%i1, g%i2
   end select

end program
