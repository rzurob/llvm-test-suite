!*  ===================================================================
!*
!*  DATE                       : 12/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : VOLATILE with OPTIONAL attribute
!*
!*  DESCRIPTION                :
!*            functional test VOLATILE compatible with OPTIONAL attribute
!* ===================================================================

    module m

       type dt1
          integer :: i
          contains
             procedure, nopass :: noOptional
             generic :: inputArgs => noOptional
       end type

       type, extends(dt1) :: dt2
          contains
             procedure, nopass :: yesOptional
             generic :: inputArgs => yesOptional
       end type

       type, extends(dt1) :: dt1ext
       end type

       type, extends(dt2) :: dt2ext
       end type

       contains
          subroutine noOptional(x,y,z)
             class(dt2), intent(in) :: x,y,z

             print *, 'noOptional'

          end subroutine

          subroutine yesOptional(x,y,z)
             class(dt1), intent(in) :: x
             class(dt2ext) :: y
             class(dt1ext), OPTIONAL, VOLATILE :: z

             print *, 'yesOptional'

          end subroutine

    end module

    program volatileOtherAttr06
       use m

       type(dt1) :: dt1_1
       type(dt2) :: dt2_1
       type(dt2ext) :: dt2ext_1
       type(dt1ext) :: dt1ext_1

       call dt2_1%inputArgs( dt1_1, dt2ext_1, dt1ext_1 )

       call dt2_1%inputArgs( dt1_1, dt2ext_1 )

    end program volatileOtherAttr06

