! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/dtybn023.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp dtybn023.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn023.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : nopass
!*
!*  DESCRIPTION                : If the interface of the binding has no
!*                               dummy argument of the type being
!*                               defined, NOPASS shall appear.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type  parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure :: bind => proc1
      end type

   contains
      subroutine proc1()
      end subroutine

   end module

!   use mod1

   end
