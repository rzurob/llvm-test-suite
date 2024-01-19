! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueSequenceAllocatableComponent001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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

   type seq(k1)    ! (4)
      integer, kind :: k1
      sequence
      integer(k1)   :: i
   end type

   type, bind(c) :: bb
      integer(C_INT) :: j
   end type

   type base(k2)    ! (4)
      integer, kind              :: k2
      type(seq(k2)), allocatable :: c
      type(bb), allocatable :: b
   end type

end module

subroutine foo ( a )
   use m, only: base
   type(base(4)), value :: a

   print *, 'foo:'
   print *, a%c%i, a%b%j

   a%c%i =-999
   a%b%j=-9999

   print *, 'foo end', a%c%i, a%b%j

end subroutine

program valueSequenceAllocatableComponent001
   use m

   type(base(4)) :: b1, b2
   allocatable :: b2

   interface
      subroutine foo ( a )
         import base
         type(base(4)), value :: a
      end subroutine
   end interface

   b1 = base(4)(seq(4)(10), bb(30))

   call foo ( base(4)(seq(4)(10), bb(20)) )
   call foo ( b1 )

   print *, b1%c%i, b1%b%j

   allocate ( b2, source = base(4)(seq(4)(-10),bb(-20)))

   call foo ( b2 )

   print *, b2%c%i, b2%b%j

end program
