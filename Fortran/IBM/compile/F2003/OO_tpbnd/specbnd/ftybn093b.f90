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
!*                               condition: both shall be functions.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod
      type base
         integer :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent1
      contains
         procedure, nopass :: bind_b => proc2
      end type

      type, extends(base) :: parent2
      contains
!* expecting the error message 1514-631
         procedure, nopass  :: bind_b => proc3
      end type

      contains
      integer function proc1()
         proc1 = 100
      end function

      integer function proc2()
         proc2 = 200
      end function

      subroutine proc3()
      end subroutine

   end module

   end

