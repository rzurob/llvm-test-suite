! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/dtybn014.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : binding attributes
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : if =>procedure-name appears, the
!*                               double-colon separator shall
!*                               appear.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure  bind => proc
      end type

      type(parent(4)) :: dt_p

      contains
      subroutine proc(arg1)
         class(parent(4)) :: arg1
      end subroutine

   end module

!   use mod1

   end

