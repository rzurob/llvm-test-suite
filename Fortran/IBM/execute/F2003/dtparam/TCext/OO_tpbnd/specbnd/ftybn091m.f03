! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/OO_tpbnd/specbnd/ftybn091m.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : Testing inheritance with nest derived types.
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
      	 procedure, nopass :: bind => proc1
	 end type

      type base2(k2,n2)    ! (4,20)
         integer, kind      :: k2
         integer, len       :: n2
         type(base1(n2,k2)) :: x
      contains
         procedure, nopass :: bind => proc2
      end type

      type base3(n3,k3)    ! (20,4)
         integer, kind :: k3
         integer, len  :: n3
         integer(k3)   :: x
      contains
         procedure, nopass :: bind => proc2
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

   type(base1(20,4)) :: dt1
   type(base2(4,20)) :: dt2
   type(base3(20,4)) :: dt3

   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt1%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   int = 0
   c = ""
   call dt2%x%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   call dt2%bind()
   if (int .ne. 200)      error stop 8
   if (c .ne. "hi")    error stop 9

   int = 0
   c = ""
   call dt3%bind()
   if (int .ne. 200)      error stop 10
   if (c .ne. "hi")    error stop 11

   end

