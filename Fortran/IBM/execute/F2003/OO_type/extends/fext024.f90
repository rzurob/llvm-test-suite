! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (empty type's extending type's
!*                               components)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    end type

    type, extends(base) :: child
        integer*4 :: ic
    end type

    type, extends (child) :: thirdGeneration
        integer*8 :: i3
    end type

    type, extends (thirdGeneration) :: fourthGeneration
        integer*2 :: i4
    end type

end module


program fext024

use m
    type(fourthGeneration) :: f4_1

    f4_1%ic = 10
    f4_1%thirdGeneration%i3 = 100
    f4_1%i4 = -1

    if (f4_1%ic /= 10) error stop 1_4

    if ((f4_1%ic /= f4_1%thirdGeneration%ic) .or. &
        (f4_1%ic /= f4_1%thirdGeneration%child%ic) .or. &
        (f4_1%ic /= f4_1%child%ic)) error stop 2_4


    if (f4_1%i4 /= -1) error stop 3_4

    if (f4_1%i3 /= 100) error stop 4_4

    if (f4_1%i3 /= f4_1%thirdGeneration%i3) error stop 5_4
end
