! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_tpbnd/specbnd/ftybn091v.f
! opt variations: -qnok -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing nopass binding has no effects
!*                               on state of the calling object
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine
   end module

   module mod2
   use mod1
      type, extends(parent) :: child(k2,n2)    ! (20,4,4,20)
          integer, kind :: k2
          integer, len  :: n2
      contains
         procedure, nopass :: bind => proc1
      end type

      type, extends(child) :: thirGen(k3,n3)    ! (20,4,4,20,4,20)
          integer, kind :: k3
          integer, len  :: n3
      contains
         procedure, nopass :: bind => proc2
      end type

      type(parent(20,4)) :: dt_p = parent(20,4)(10)
      type(child(20,4,4,20)) :: dt_c = child(20,4,4,20)(20)
      type(thirGen(20,4,4,20,4,20)) :: dt_g3 = thirGen(20,4,4,20,4,20)(30)

      contains
      subroutine proc2()
         int = 0
         c = "hi"
      end subroutine
   end module

   use mod2

   if (int .ne. 200)       error stop 2
   if (c .ne. "hi")       error stop 3

   call dt_p%bind()
   if (int .ne. 400)       error stop 4
   if (c .ne. "hi_again") error stop 5
   if (dt_p%x .ne. 10)     error stop 55

   call proc2()
   call dt_c%bind()
   if (int .ne. 400)       error stop 6
   if (c .ne. "hi_again") error stop 7
   if (dt_c%x .ne. 20)     error stop 77

   call dt_g3%bind()
   if (int .ne. 0)      error stop 8
   if (c .ne. "hi")    error stop 9
   if (dt_g3%x .ne. 30) error stop 99

   end


