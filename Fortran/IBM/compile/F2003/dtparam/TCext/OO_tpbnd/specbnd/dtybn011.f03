! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/dtybn011.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : binding attributes
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : the same binding attribute shall not
!*                               appear more than once in a given
!*                               binding-attr-list.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, private, pass, non_overridable, pass, pass, non_overridable :: bind
      end type

      type(parent(20,4)) :: dt_p

      contains
      subroutine bind(arg1)
         class(parent(*,4)) :: arg1
      end subroutine

   subroutine test
      call dt_p%bind()
   end subroutine

   end module

   use mod1

   call test

   end
