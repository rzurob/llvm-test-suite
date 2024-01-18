! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_tpbnd/specbnd/ftybn093b.f
! opt variations: -qnok -qnol

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
      type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent1(k2,n2)    ! (20,4,4,20)
          integer, kind :: k2
          integer, len  :: n2
      contains
         procedure, nopass :: bind_b => proc2
      end type

      type, extends(base) :: parent2(k3,n3)    ! (20,4,4,20)
          integer, kind :: k3
          integer, len  :: n3
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

