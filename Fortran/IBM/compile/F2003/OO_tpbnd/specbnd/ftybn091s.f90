!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp ftybn091s.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091s.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable
!*
!*  DESCRIPTION                : testing the parent procedures are
!*                               overridden, with multiple levels
!*                               overridden.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type parent
         integer :: x
	 contains
      	 procedure, nopass, non_overridable :: bind => proc1
         procedure, nopass :: bind_r => proc2
      end type

      type, extends(parent) :: child
      contains
         procedure, nopass, non_overridable :: bind => proc1
      end type

      type, extends(child) :: thirGen
      contains
         procedure, nopass, non_overridable :: bind => proc1
      end type

      type, extends(thirGen) :: fourGen
      contains
         procedure, nopass, non_overridable :: bind => proc1
      end type

      type, extends(fourGen) :: fifGen
      contains
         procedure, nopass, non_overridable :: bind => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = ""
      end subroutine

   end module

   end

