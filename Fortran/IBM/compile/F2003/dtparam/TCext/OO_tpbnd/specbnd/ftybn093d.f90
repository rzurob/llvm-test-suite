! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn093d.f
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
!*                               condition: both shall be functions having
!*                               the same result characteristics.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      type base(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
      contains
      	 procedure, pass :: bind_b => proc1
      end type

      type, extends(base) :: child    ! (4)
      contains
         procedure, pass :: bind_b => proc2
      end type

      contains
      function proc1(arg1) result(arg2)
          class(base(4)), intent(in) :: arg1
          integer :: arg2
          arg2 = 100
      end function

      function proc2(arg1) result(arg2)
          class(child(4)), intent(in) :: arg1
          character*20 :: arg2
          arg2 = "hello"
      end function

   end module

   end

