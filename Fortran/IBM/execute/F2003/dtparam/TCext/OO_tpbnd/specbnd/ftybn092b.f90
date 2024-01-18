! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn092b.f
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
!*  DESCRIPTION                : testing accessiblity overriding with two
!*                               types which all extend from the base
!*                               type, but overriding the type-bound
!*                               procedures of the base type differently.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      integer :: int = 200
      character*20 :: char = "hi"

      type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, nopass, private :: bind_b => proc1
      end type

      type, extends(base) :: parent1    ! (20,4)
      contains
         procedure, nopass, public :: bind_b => proc1
      end type

!*   if no private attribute specifies, the default is public
!*   accessiblity

      type, extends(base) :: parent2    ! (20,4)
      contains
         procedure, nopass  :: bind_b => proc1
      end type

      type(base(20,4)) :: dt

      contains
      subroutine proc1()
         int = 400
         char = "hi_again"
      end subroutine

      subroutine proc2(arg1)
         type(base(*,4)) :: arg1
         call arg1%bind_b()
      end subroutine

   end module

   use mod

   type(base(20,4))    :: dt
   type(parent1(20,4)) :: dt_p1
   type(parent2(20,4)) :: dt_p2
   if (int .ne. 200)      error stop 2
   if (char .ne. "hi")    error stop 3

   call proc2(dt)
   if (int .ne. 400)      error stop 4
   if (char .ne. "hi_again")    error stop 5

   int = 0
   char = ""
   call dt_p1%bind_b()
   if (int .ne. 400)      error stop 6
   if (char .ne. "hi_again")    error stop 7

   int = 0
   char = ""
   call dt_p2%bind_b()
   if (int .ne. 400)      error stop 8
   if (char .ne. "hi_again")    error stop 9

   end

