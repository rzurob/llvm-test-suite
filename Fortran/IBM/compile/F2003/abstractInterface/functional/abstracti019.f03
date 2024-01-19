! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), function return cannot be abstract type, type(abstract type)
!*                                        Return Scalar abstract type object, pointer
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

   type, abstract :: base
      integer :: id
   contains
      procedure(fooif), nopass, deferred :: foo
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: foo
   end type

   abstract interface
      type(base) function fooif()
         import base
      end function
   end interface

contains

   type(base) function foo()
      foo = base(4)
   end function

   function boo() result(boo1)
      type(base), pointer :: boo1
      boo1 => null()
   end function

end module

program abstracti019

end program abstracti019

