!######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp final001.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Final Subroutines
!*                               Define Final Subroutine for abstract type
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
   type , abstract:: base1
      integer :: i
   contains
      final :: finalbase1
   end type

   type, abstract :: base2
      integer :: k
   contains
      final :: finalbase2
   end type

contains

   subroutine finalbase1(a)
      type(base1), intent(in) :: a
      print *,"finalizebase"
   end subroutine

   subroutine finalbase2(a)
      class(base2), intent(in) :: a
      print *,"finalizebase"
   end subroutine

end module

program final001

end program

