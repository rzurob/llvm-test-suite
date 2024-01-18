! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn091i.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn091i.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091i.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : testing a procedure is bound to two different
!*                               types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type base1(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
      contains
      	 procedure, nopass :: bind_b1 => proc1
      end type

      type base2(n2,k2)    ! (20,4)
         integer, kind :: k2
         integer, len  :: n2
         integer(k2)   :: x
      contains
         procedure, nopass :: bind_b2 => proc1
      end type

      type base3(n3,k3)    ! (20,4)
         integer, kind :: k3
         integer, len  :: n3
         integer(k3)   :: x
      contains
         procedure, nopass :: bind_b3 => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

	end module

   use mod

   type(base1(20,4)) :: dt1
   type(base2(20,4)) :: dt2
   type(base3(20,4)) :: dt3
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt1%bind_b1()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   int = 0
   c = ""
   call dt2%bind_b2()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   int = 0
   c = ""
   call dt3%bind_b3()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end

