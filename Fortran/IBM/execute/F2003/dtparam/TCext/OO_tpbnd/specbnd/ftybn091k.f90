! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn091k.f
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
!*  DESCRIPTION                : testing the base procedure is bound to
!*                               two types which all extend the base type,
!*                               with different binding-names.
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
      end type

      type, extends(base) :: parent1    ! (20,4)
      contains
	 procedure, nopass :: bind_p1 => proc1
      end type

      type, extends(base) :: parent2    ! (20,4)
      contains
         procedure, nopass :: bind_p2 => proc1
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
   type(parent1(20,4)) :: dt_p1
   type(parent2(20,4)) :: dt_p2

   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   call proc2()
   call dt_p1%bind_p1()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   call proc2()
   call dt_p2%bind_p2()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   end

