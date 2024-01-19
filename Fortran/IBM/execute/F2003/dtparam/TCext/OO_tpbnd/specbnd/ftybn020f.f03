! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn020f.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private derived type
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable
!*
!*  DESCRIPTION                : testing the type bound procedure
!*                               which within a private derived type
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type,private :: parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, pass, non_overridable :: bind => proc1
      end type

   type(parent(20,4)) :: dt_p

      contains
      subroutine proc1(arg1)
         class(parent(*,4)) :: arg1
         arg1%x = 200
      end subroutine

   end module

   use mod1
   call dt_p%bind()

   if ( dt_p%x .ne. 200) error stop 2_4

   end

