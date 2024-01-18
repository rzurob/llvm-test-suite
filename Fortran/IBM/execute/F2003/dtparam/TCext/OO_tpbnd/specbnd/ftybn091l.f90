! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn091l.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn091l.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091l.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : testing the base procedure is bound to
!*                               multiple level inherited types with
!*                               different binding-names.
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
         procedure, nopass :: bind_r => proc2
	 end type

      type, extends(base) :: parent    ! (20,4)
      contains
         procedure, nopass :: bind_p => proc1
      end type

      type, extends(parent) :: child    ! (20,4)
      contains
         procedure, nopass :: bind_c => proc1
      end type

      type, extends(child) :: thirGen    ! (20,4)
      contains
         procedure, nopass :: bind_3 => proc1
      end type

      type, extends(thirGen) :: fourGen    ! (20,4)
      contains
         procedure, nopass :: bind_4 => proc1
      end type

      type, extends(fourGen) :: fifGen    ! (20,4)
      contains
         procedure, nopass :: bind_5 => proc1
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
   call dt_p%bind_p()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   call dt_c%bind_r()
   call dt_c%bind_c()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   call dt_3%bind_r()
   call dt_3%bind_3()
   if (int .ne. 400)      error stop 12
   if (c .ne. "hi_again")    error stop 13

   call dt_4%bind_r()
   call dt_4%bind_4()
   if (int .ne. 400)      error stop 14
   if (c .ne. "hi_again")    error stop 15

   call dt_5%bind_r()
   call dt_5%bind_5()
   if (int .ne. 400)      error stop 16
   if (c .ne. "hi_again")    error stop 17

   end

