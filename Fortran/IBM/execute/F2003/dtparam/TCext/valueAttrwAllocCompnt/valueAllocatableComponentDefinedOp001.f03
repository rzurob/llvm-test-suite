! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentDefinedOp001.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with inttrinsic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - with use defined operator
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
         procedure, pass :: add
         procedure, pass :: addint
         generic :: operator(+) => add, addint
   end type

   contains

   type(base(4)) function add ( a, b )
      class(base(4)), intent(in) :: a
      type(base(4)), value, intent(in) :: b

      allocate ( add%i )
      add%i = a%i + b%i

   end function

   type(base(4)) function addint ( a, b )
      class(base(4)), intent(in) :: a
      integer, value, intent(in) :: b

      allocate ( addint%i )
      addint%i = a%i + b

   end function

end module

program valueAllocatableComponentDefinedOp001
   use m

   type(base(4)) :: b1
   b1 = base(4)(100) + base(4)(200)
   print *, b1%i

   b1 = b1 + b1
   print *, b1%i

   b1 = b1 + ( -300 )
   print *, b1%i

end program