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
! %POSTCMD: dcomp implicit001.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Abstract type with IMPLICIT STATEMENT
!*                                        IMPLICIT illegal abstract type
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
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: print
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

   IMPLICIT type(base) (A-F)

   pointer A1
   allocatable B1
contains
   subroutine print()
      print *,'base'
   end subroutine
end module

program implicit001

end program

