! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/dtybn003.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable
!*
!*  DESCRIPTION                : a binding-private-stmt is permitted only
!*                               if the type definition is within the
!*                               specification part of a module.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure,private, non_overridable :: bind => proc1
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
