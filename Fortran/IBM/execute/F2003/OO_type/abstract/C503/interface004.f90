!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: interface004.f
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
!*  DESCRIPTION                : Testing: interface block
!*                                        poly abstract type return, interface of an external procedure
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
end type

type, extends(base) :: child
end type

interface
   class(base) function itf(a)
      import base
      class(base), intent(in) :: a
      pointer :: itf
   end function
end interface

end module

program interface004
   use m

   class(base), pointer :: b1
   type(child) :: c1 = child(5)
   procedure(itf) :: getbase

   b1 => getbase(c1)
   if(b1%id .ne. 5) error stop 1_4

end program

class(base) function getbase(a)
   use m
   class(child), intent(in) :: a
   pointer getbase
   allocate (getbase,source=a)
end function
