!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: deferred003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Overridding
!*                                        deferred binding overridden by non-overridable procedure
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
   procedure(itf), pass, deferred :: getid
end type

type, extends(base) :: child
contains
   procedure, pass, non_overridable :: getid
end type

interface
   integer function itf(a)
      import base
      class(base), intent(in) :: a
   end function
end interface

contains

   integer function getid(a)
      class(child), intent(in) :: a
      getid = a%id
   end function

end module

program deferred003
   use m

   class(base), allocatable :: b1

   allocate(b1, source = child(10))
   if (b1%getid() .ne. 10) error stop 1_4

end program