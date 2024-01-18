!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - with sequence type allocatable component
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

   use ISO_C_BINDING

   type seq
      sequence
      integer :: i
   end type

   type, bind(c) :: bb
      integer(C_INT) :: j
   end type

   type base
      type(seq), allocatable :: c
      type(bb), allocatable :: b
   end type

end module

subroutine foo ( a )
   use m, only: base
   type(base), value :: a

   print *, 'foo:'
   print *, a%c%i, a%b%j

   a%c%i =-999
   a%b%j=-9999

   print *, 'foo end', a%c%i, a%b%j

end subroutine

program valueSequenceAllocatableComponent001
   use m

   type(base) :: b1, b2
   allocatable :: b2

   interface
      subroutine foo ( a )
         import base
         type(base), value :: a
      end subroutine
   end interface

   b1 = base(seq(10), bb(30))

   call foo ( base(seq(10), bb(20)) )
   call foo ( b1 )

   print *, b1%c%i, b1%b%j

   allocate ( b2, source = base(seq(-10),bb(-20)))

   call foo ( b2 )

   print *, b2%c%i, b2%b%j

end program
