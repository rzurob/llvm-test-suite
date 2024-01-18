! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn093g.f
! opt variations: -ql

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
!*                               condition: either both shall be elemental
!*                               or neither shall.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod
      type base(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent    ! (4)
      contains
         procedure, nopass :: bind_b => proc2
      end type

      contains
      elemental function proc1(arg1)
         integer, intent(in) :: arg1
         proc1 = 100
      end function

!* expected error message here
      function proc2(arg1)
         integer, intent(in) :: arg1
         proc2 = 200
      end function

   end module

   end

