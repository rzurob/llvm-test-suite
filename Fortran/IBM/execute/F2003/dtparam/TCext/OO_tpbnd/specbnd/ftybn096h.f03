! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn096h.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               with multiple levels inheritance.
!*                               inherite from a different scoping units.
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
      	 procedure, pass :: bind_b => proc1
         procedure, pass :: bind_r => proc2
      end type

      contains
      subroutine proc1(arg1)
         class(base(*,4)) :: arg1
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2(arg1)
         class(base(*,4)) :: arg1
         int = 0
         c = ""
      end subroutine

   end module

   module mod1
      use mod
      type, extends(base) :: parent    ! (20,4)
      end type
   end module

   module mod2
      use mod1
      type, extends(parent) :: child    ! (20,4)
      end type
   end module

   module mod3
      use mod2
      type, extends(child) :: thirGen    ! (20,4)
      end type
   end module

   module mod4
      use mod3
      type, extends(thirGen) :: fourGen    ! (20,4)
      end type
   end module

   module mod5
      use mod4
      type, extends(fourGen) :: fifGen    ! (20,4)
      end type
   end module

   use mod5

   type(base(20,4)) :: dt
   type(parent(20,4)) :: dt_p
   type(child(20,4)) :: dt_c
   type(thirGen(20,4)) :: dt_3
   type(fourGen(20,4)) :: dt_4
   type(fifGen(20,4)) :: dt_5
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_p%bind_r()
   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_c%bind_r()
   call dt_c%bind_b()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   call dt_3%bind_r()
   call dt_3%bind_b()
   if (int .ne. 400)      error stop 12
   if (c .ne. "hi_again")    error stop 13

   call dt_4%bind_r()
   call dt_4%bind_b()
   if (int .ne. 400)      error stop 14
   if (c .ne. "hi_again")    error stop 15

   call dt_5%bind_r()
   call dt_5%bind_b()
   if (int .ne. 400)      error stop 16
   if (c .ne. "hi_again")    error stop 17

   end
