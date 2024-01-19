! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/volatile/test/volatileOtherAttr05.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 12/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : VOLATILE with SAVE attribute
!*
!*  DESCRIPTION                :
!*            functional test VOLATILE compatible with SAVE attribute
!* ===================================================================

   module m
      type dt(k1)    ! (4)
         integer, kind :: k1
         integer(k1)      x
      end type

      contains

      function func(y)
        type(dt(4)), intent(in) :: y
        type(dt(4)), pointer :: func

        type(dt(4)), target, SAVE, VOLATILE::tmp

        tmp%x = y%x

        func=>tmp
      end function
   end module

   program volatileOtherAttr05
     use m
     type(dt(4)), SAVE, VOLATILE :: a, b
     a%x = 20
     b = func(a)

  end program volatileOtherAttr05
