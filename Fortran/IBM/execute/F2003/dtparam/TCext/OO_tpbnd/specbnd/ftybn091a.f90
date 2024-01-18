! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn091a.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod
         integer :: int = 200
         character*20 :: c = "hi"

      type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
		contains
      	procedure, nopass :: bind_b => proc1
      end type base

      type, extends(base) :: parent    ! (20,4)
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

	end module

   use mod

   type(base(20,4)) :: dt
   type(parent(20,4)) :: dt_p
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end

