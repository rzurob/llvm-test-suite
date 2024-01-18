! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/dtybn008.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private subroutine
!*
!*  SECONDARY FUNCTIONS TESTED : nopass , non_overridable
!*
!*  DESCRIPTION                : testing the private type bound
!*                               procedure within a private derived
!*                               type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type,private :: parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, private, nopass, non_overridable :: bind => proc1
      end type

   type(parent(4)) :: dt_p

      contains
      subroutine proc1()
      end subroutine

   end module

   use mod1
   call dt_p%bind()

   end

