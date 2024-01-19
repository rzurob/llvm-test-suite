! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn020d.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private derived type
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : testing the type bound procedure
!*                               which within a private derived type
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type,private :: parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, pass :: bind => proc1
      end type

   type(parent(4)) :: dt_p

      contains
      subroutine proc1(arg1)
         class(parent(4)) :: arg1
         arg1%x = 200
      end subroutine

   end module

   use mod1
   call dt_p%bind()

   if ( dt_p%x .ne. 200) error stop 2_4

   end

