! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn020c.f
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
!*  DESCRIPTION                : testing type bound procedure with
!*                               private attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure,private, pass,non_overridable :: bind => proc1
      end type

   type(parent(20,4)) :: dt_p

      contains
      subroutine proc1(arg1)
         class(parent(*,4)) :: arg1
         arg1%x = 100
      end subroutine

   subroutine test1
      call dt_p%bind()
   end subroutine

   end module

   use mod1
   call test1
   if (dt_p%x .ne. 100)   error stop 2_4

   end

