! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn093c.f
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
!*  DESCRIPTION                : The overriding binding and the overriden
!*                               binding shall satisfy the following
!*                               condition: both shall be functions having
!*                               the same result characteristics.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod
      type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent1    ! (20,4)
      contains
         procedure, nopass :: bind_b => proc2
      end type

      type, extends(base) :: parent2    ! (20,4)
      contains
         procedure, nopass  :: bind_b => proc3
      end type

      contains
      function proc1() result(arg1)
         integer :: arg1
         arg1 = 100
      end function

      function proc2() result(arg1)
         character*20 :: arg1
         arg1 = "hello"
      end function

      subroutine proc3()
      end subroutine

   end module

   end

