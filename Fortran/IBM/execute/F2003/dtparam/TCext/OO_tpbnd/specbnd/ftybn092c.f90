! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn092c.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : accessibility
!*
!*  DESCRIPTION                : testing the accessiblity of parent's
!*                               type-bound procedures are overrided by
!*                               the multiple generations extended types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      integer :: int = 200
      character*20 :: c = "hi"

      type base(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, nopass, private :: bind_b => proc1
      end type

      type, extends(base) :: parent    ! (4)
      contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(parent) :: child    ! (4)
      contains
         procedure, nopass, public :: bind_b => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

   end module

   use mod

   type(child(4)) :: dt_c
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_c%bind_b()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   end

