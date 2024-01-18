! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/OO_tpbnd/specbnd/ftybn091r.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn091r.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091r.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable
!*
!*  DESCRIPTION                : Testing inheritance with non_overridable
!*                               attribute.e.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type base1(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, nopass, non_overridable :: bind => proc1
      end type

      type base2(k2)    ! (4)
         integer, kind   :: k2
         type(base1(k2)) :: x
      contains
         procedure, nopass, non_overridable ::  proc1
      end type

      type base3(k3)    ! (4)
         integer, kind   :: k3
         type(base2(k3)) :: x
      contains
         procedure, nopass, non_overridable :: bind => proc2
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 200
         c = "hi"
      end subroutine

   end module

   use mod

   type(base1(4)) :: dt1
   type(base2(4)) :: dt2
   type(base3(4)) :: dt3

   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt1%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   int = 0
   c = ""
   call dt2%proc1()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   int = 0
   c = ""
   call dt2%x%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   int = 0
   c = ""
   call dt3%bind()
   if (int .ne. 200)      error stop 10
   if (c .ne. "hi")    error stop 11

   call dt3%x%x%bind()
   if (int .ne. 400)      error stop 11
   if (c .ne. "hi_again")    error stop 12

   end

