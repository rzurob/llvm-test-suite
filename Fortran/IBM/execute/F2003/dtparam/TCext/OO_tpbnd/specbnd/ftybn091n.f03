! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn091n.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : testing parent procedures are overriden,
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind => proc1
      end type

      type, extends(parent) :: child    ! (4)
      contains
         procedure, nopass :: bind => proc1
      end type

      type, extends(child) :: thirGen    ! (4)
      contains
         procedure, nopass :: bind => proc2
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = "hi"
      end subroutine

	end module

   use mod

   type(parent(4)) :: dt_p
   type(child(4)) :: dt_c
   type(thirGen(4)) :: dt_g3

   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_p%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   call proc2()
   call dt_c%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   call dt_g3%bind()
   if (int .ne. 0)      error stop 8
   if (c .ne. "hi")    error stop 9

   end
