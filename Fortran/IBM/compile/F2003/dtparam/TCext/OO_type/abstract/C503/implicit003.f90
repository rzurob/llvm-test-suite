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
! %POSTCMD: dcomp implicit003.f
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
!*                                        IMPLICIT nonpolymorphic abstract type
!*                                        try to use it as actual argument with polymorphic abstract type dummy arg
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure, nopass :: print
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
   end type

contains
   subroutine print()
      print *,'hello'
   end subroutine

   subroutine foo(a)
      class(base(4)) :: a
      print *,a%id
   end subroutine

end module

program implicit003
   use m
   IMPLICIT type(base(4)) (A-F)
   call foo(Aa)

end program
