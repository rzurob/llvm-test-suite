! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/volatile/test/volatileOtherAttr06.f
! opt variations: -ql

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

       type dt1(k1)    ! (4)
          integer, kind :: k1
          integer(k1)   :: i
          contains
             procedure, nopass :: noOptional
             generic :: inputArgs => noOptional
       end type

       type, extends(dt1) :: dt2    ! (4)
          contains
             procedure, nopass :: yesOptional
             generic :: inputArgs => yesOptional
       end type

       type, extends(dt1) :: dt1ext    ! (4)
       end type

       type, extends(dt2) :: dt2ext    ! (4)
       end type

       contains
          subroutine noOptional(x,y,z)
             class(dt2(4)), intent(in) :: x,y,z

             print *, 'noOptional'

          end subroutine

          subroutine yesOptional(x,y,z)
             class(dt1(4)), intent(in) :: x
             class(dt2ext(4)) :: y
             class(dt1ext(4)), OPTIONAL, VOLATILE :: z

             print *, 'yesOptional'

          end subroutine

    end module

    program volatileOtherAttr06
       use m

       type(dt1(4)) :: dt1_1
       type(dt2(4)) :: dt2_1
       type(dt2ext(4)) :: dt2ext_1
       type(dt1ext(4)) :: dt1ext_1

       call dt2_1%inputArgs( dt1_1, dt2ext_1, dt1ext_1 )

       call dt2_1%inputArgs( dt1_1, dt2ext_1 )

    end program volatileOtherAttr06

