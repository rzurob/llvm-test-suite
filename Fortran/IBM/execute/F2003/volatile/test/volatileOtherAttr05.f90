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
      type dt
         integer x
      end type

      contains

      function func(y)
        type(dt), intent(in) :: y
        type(dt), pointer :: func

        type(dt), target, SAVE, VOLATILE::tmp

        tmp%x = y%x

        func=>tmp
      end function
   end module

   program volatileOtherAttr05
     use m
     type(dt), SAVE, VOLATILE :: a, b
     a%x = 20
     b = func(a)

  end program volatileOtherAttr05
