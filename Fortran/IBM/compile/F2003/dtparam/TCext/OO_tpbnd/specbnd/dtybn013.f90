! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/dtybn013.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : binding attributes
!*
!*  SECONDARY FUNCTIONS TESTED : pass, nopass, non_overridable
!*
!*  DESCRIPTION                : pass and nopass shall not both appear
!*                               in the same binding-attr-list.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, private, nopass, non_overridable, pass, nopass, non_overridable :: bind
      end type

      type(parent(20,4)) :: dt_p

      contains
      subroutine bind(arg1)
         class(parent(20,4)) :: arg1
      end subroutine

   subroutine test
      call dt_p%bind(dt_p)
   end subroutine

   end module

!   use mod1

!   call test

   end

