! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn093aa.f
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
!*                               condition: both shall be subroutines
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
      end type base

      type, extends(base) :: parent    ! (20,4)
      end type

      type, extends(parent) :: child    ! (20,4)
      contains
!* expect the error massage 1514-631
         procedure, nopass  :: bind_b => proc3
      end type

      contains
      subroutine proc1()
      end subroutine

      integer function proc3()
         proc3 = 100
      end function

   end module

   end
