! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Overridding
!*                                        deferred binding overridden by final procedure
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
   integer id
contains
   procedure(itf), pass, deferred :: final
end type

type, extends(base) :: child
contains
   final :: final
end type

interface
   subroutine itf(a)
      import base
      class(base), intent(inout) :: a
   end subroutine
end interface

contains
   subroutine final(a)
      type(child), intent(inout) :: a
      a%id=0
      print *,'finalizechild'
   end subroutine
end module

program deferred004

end program