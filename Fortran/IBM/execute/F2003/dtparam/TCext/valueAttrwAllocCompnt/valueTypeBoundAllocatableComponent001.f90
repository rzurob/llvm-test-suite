! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueTypeBoundAllocatableComponent001.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable) with optional attribute
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - function is type bound procedure
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

   type base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
      contains
         procedure, nopass :: myfoo => foo
   end type

   type, extends(base) :: child    ! (4)
      integer(k1), allocatable :: j
   end type

   contains

   subroutine foo ( a, b )
      type(base(4)), value :: a
      type(child(4)), value :: b

      print *, 'foo:'
      print *, a%i, b%i, b%j
      a%i = -999
      b%i = -999
      b%j = -999
      print *, a%i, b%i, b%j

   end subroutine

end module

program valueTypeBoundAllocatableComponent001
   use m

   type(base(4)) :: b1
   type(base(4)), allocatable :: b2

   type(child(4)) :: c1
   type(child(4)), pointer :: c2

   b1 = base(4)(100)
   allocate ( b2, source = base(4)(200) )

   c1 = child(4)(300,400)
   allocate ( c2, source = child(4)(500,600) )

   call b1%myfoo(b1, c1)
   print *, b1%i, c1%i, c1%j

   call c1%myfoo(b2, c2)
   print *, b2%i, c2%i, c2%j

   call foo(b1, c1)
   print *, b1%i, c1%i, c1%j

   call foo(b2, c2)
   print *, b2%i, c2%i, c2%j

end program
