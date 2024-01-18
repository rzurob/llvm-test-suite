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

   type base
      integer, allocatable :: i
      contains
         procedure, pass :: add
         procedure, pass :: addint
         generic :: operator(+) => add, addint
   end type

   contains

   type(base) function add ( a, b )
      class(base), intent(in) :: a
      type(base), value, intent(in) :: b

      allocate ( add%i )
      add%i = a%i + b%i

   end function

   type(base) function addint ( a, b )
      class(base), intent(in) :: a
      integer, value, intent(in) :: b

      allocate ( addint%i )
      addint%i = a%i + b

   end function

end module

program valueAllocatableComponentDefinedOp001
   use m

   type(base) :: b1
   b1 = base(100) + base(200)
   print *, b1%i

   b1 = b1 + b1
   print *, b1%i

   b1 = b1 + ( -300 )
   print *, b1%i

end program
