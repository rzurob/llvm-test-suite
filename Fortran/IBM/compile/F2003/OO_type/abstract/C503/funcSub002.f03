! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), function return cannot be abstract type, type(abstract type)
!*                                        Return abstract type array object
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

   interface
      type(base) function fooif()
         import base
         dimension :: fooif(0)
      end function
   end interface

contains

   type(base) function foo()
      dimension :: foo(0)
      foo = (/ (base(1),i=1,0) /)
   end function

   function foo1() result(myfoo)
      type(base), pointer :: myfoo(:)
      myfoo => null()
   end function

end module

program funcSub002

end program

