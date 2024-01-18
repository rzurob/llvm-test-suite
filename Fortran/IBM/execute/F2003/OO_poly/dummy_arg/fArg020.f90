!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg020.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (array element ordering)
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
        integer*4 :: id
    end type

    contains

    integer*4 function sumPart (b, vec)
        class (base), intent(in) :: b(2:6)
        integer, intent(in) :: vec(:)

        sumPart = sum (b(vec)%id)
    end function
end module


program fArg020
use m
    type (base), allocatable :: b1 (:)
    class (base), allocatable :: b2(:)
    type (base) :: b3 (2, 5)

    allocate (b1(5))

    b1%id = (/(i, i=10,14)/)

    if (sumPart (b1, (/3,4,5/)) /= 36) error stop 1_4

    if (sumPart ((/base(1), base(2), base(3), base(4), base(5)/), (/4,3/)) &
                /= 5) error stop 2_4

    allocate (b2 (3:10))

    b2%id = (/3,4,5,6,7,8,9,10/)

    if (sumPart (b2(6:), (/6/)) /= 10) error stop 3_4

    b3 = reshape ((/(base(i),i=1,10)/), (/2,5/))

    if (sumPart (b3(2,:), (/3,2/)) /= (2+4)) error stop 4_4

    if (sumPart (b3(1,:), (/(i,i=1,-1)/)) /= 0) error stop 5_4
end
