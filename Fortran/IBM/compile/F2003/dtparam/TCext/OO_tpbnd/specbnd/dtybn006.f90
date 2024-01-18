! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/dtybn006.f
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
! %POSTCMD: dcomp dtybn006.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn006.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private subroutine
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable
!*
!*  DESCRIPTION                : call the private subroutine in a different
!*                               program unit by using a public
!*                               type bound procedure.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      private proc1
      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, non_overridable :: bind => proc1
      end type

      contains
      subroutine proc1(arg1)
         class(parent(*,4)) :: arg1
      end subroutine
   end module

   use mod1

   type(parent(20,4)) :: dt_p
   call dt_p%bind()

   end

