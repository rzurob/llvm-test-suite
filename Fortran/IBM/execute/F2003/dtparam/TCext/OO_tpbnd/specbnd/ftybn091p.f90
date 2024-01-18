! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_tpbnd/specbnd/ftybn091p.f
! opt variations: -qnok -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn091p.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091p.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : testing the parent procedures are
!*                               overridden, with multiple levels
!*                               overridden.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind => proc1
         procedure, nopass :: bind_r => proc2
      end type

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
         procedure, nopass :: bind => proc1
      end type

      type, extends(thirGen) :: fourGen(k4,n4)    ! (20,4,4,20,4,20,4,20)
          integer, kind :: k4
          integer, len  :: n4
      contains
         procedure, nopass :: bind => proc1
      end type

      type, extends(fourGen) :: fifGen(k5,n5)    ! (20,4,4,20,4,20,4,20,4,20)
          integer, kind :: k5
          integer, len  :: n5
      contains
         procedure, nopass :: bind => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = ""
      end subroutine

	end module

   use mod

   type(parent(20,4)) :: dt_p
   type(child(20,4,4,20)) :: dt_c
   type(thirGen(20,4,4,20,4,20)) :: dt_3
   type(fourGen(20,4,4,20,4,20,4,20)) :: dt_4
   type(fifGen(20,4,4,20,4,20,4,20,4,20)) :: dt_5

   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_p%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   call dt_c%bind_r()
   call dt_c%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   call dt_3%bind_r()
   call dt_3%bind()
   if (int .ne. 400)      error stop 12
   if (c .ne. "hi_again")    error stop 13

   call dt_4%bind_r()
   call dt_4%bind()
   if (int .ne. 400)      error stop 14
   if (c .ne. "hi_again")    error stop 15

   call dt_5%bind_r()
   call dt_5%bind()
   if (int .ne. 400)      error stop 16
   if (c .ne. "hi_again")    error stop 17

   end

