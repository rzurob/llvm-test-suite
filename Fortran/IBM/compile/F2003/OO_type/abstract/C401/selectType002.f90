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
! %POSTCMD: dcomp selectType002.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: the derived-type-spec shall not specify an ABSTRACT type (C401)
!*                                        Select Type Construct: type-guard-stmt TYPE IS specifies type-spec is Abstract type
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

   type, abstract :: b1
      integer :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b1) :: b2
   contains
      procedure, nopass :: print
   end type

   interface
      subroutine printif()
      end subroutine
   end interface
contains
   subroutine print()
      print *,'b2'
   end subroutine
end module

program selectType002
   use m
   class(b1), allocatable :: b11
   allocate(b2 :: b11)

   select type ( b => b11 )
      type is (b1)
         error stop 1_4
      class default
   end select

end program
