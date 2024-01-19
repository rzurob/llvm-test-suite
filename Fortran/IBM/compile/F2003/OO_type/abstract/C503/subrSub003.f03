! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Subroutine subprogram (Section 12.5.2.1)
!*                               prefix shall not contain declaration-type-spec
!*                               dummyarg as polymorphic abstract type of a type-bound procedure and with deferred binding (interface)
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
      procedure(fooinf), nopass, deferred :: foo
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: foo
   end type

   interface
      subroutine fooinf(dtv)
         import base
         type(base) :: dtv
      end subroutine
   end interface

contains
   subroutine foo(dtv)
      type(base) :: dtv
      print *,"error"
   end subroutine
end module


program subrSub003

end program