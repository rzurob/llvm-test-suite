! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (sequence component in
!*                               the structure constructor)
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
    type seq1
        sequence
        integer*4 :: x = 1
        integer*4 :: y = 1
    end type
end module

module m1
use m
    type base
        type (seq1) :: s1
    end type

    type (seq1), save :: s1_m = seq1()
end module

program fconstr040

use m1
    interface
        subroutine updateBase (b)
        use m1, only : base
            type (base), intent(inout) :: b
        end subroutine
    end interface

    type (base) :: b1

    b1 = base (s1 = s1_m)

    if ((b1%s1%x /= 1) .or. (b1%s1%y /= 1)) error stop 1_4

    call updateBase (b1)

    if ((b1%s1%x /= 1) .or. (b1%s1%y /= 10)) error stop 2_4
end

subroutine updateBase (b)
use m1, only : base
    type (base), intent(inout) :: b

    type seq1
        sequence
        integer*4 :: x = 10
        integer*4 :: y = 10
    end type

    type (seq1) :: s1

    if ((s1%x /= 10) .or. (s1%y /= 10)) error stop 10_4

    s1 = seq1(y= 10, x = 1)

    b = base(s1 = s1)
end subroutine
