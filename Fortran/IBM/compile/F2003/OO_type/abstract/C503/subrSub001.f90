!#######################################################################
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
! %POSTCMD: dcomp subrSub001.f
! %END
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
!*                               declaration-type-spec as polymorphic abstract type
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
   contains
      procedure, nopass :: foo
   end type

contains

   recursive subroutine foo(dtv)
      type(base), intent(inout) :: dtv
      print *,"error"
   end subroutine

   type(base) elemental subroutine boo()
   end subroutine

end module


program subrSub001

end program