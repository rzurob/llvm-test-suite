! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-dummy-arg
!*                               associated with sequence type)
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

        integer*4 :: i1
        integer*4 i2
    end type
end module

program fArg004
use m
    interface
        subroutine test1 (x)
            class(*), intent(in), target :: x
        end subroutine
    end interface

    interface
        integer*4 function test2 (x)
            class (*), intent(in), target :: x(:)
        end function
    end interface

    type (seq1) :: s1(2)

    call test1 (seq1 (1, i2 = 10))

    if (test2 ((/seq1(1,1), seq1(1,2)/)) /= 5) error stop 1_4

    s1 = (/seq1(1,1), seq1(2,2)/)

    if (test2 (x = s1) /= 6) error stop 2_4
end

subroutine test1 (x)
    class (*), intent(in), target :: x

    type seq1
        sequence
        integer*4 :: i1
        integer*4 i2
    end type

    type (seq1), pointer :: s1

    s1 => x

    print *, s1
end subroutine

integer*4 function test2 (x)
    class (*), intent(in), target :: x(:)

    type seq1
        sequence
        integer*4 :: i1
        integer*4 i2
    end type

    type (seq1), pointer :: s1 (:)

    s1 => x

    test2 = sum (s1%i1 + s1%i2)
end function
