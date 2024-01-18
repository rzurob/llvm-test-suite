! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-function
!                               return; use sequence type to test)
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
    contains

    class (*) function replicateAnything (x)
        class (*), intent(in) :: x

        pointer replicateAnything

        allocate (replicateAnything, source=x)
    end function

    subroutine testSeq (x)
        class (*), intent(inout), target :: x

        type seq1
            sequence

            integer*4 i1
            integer*2 :: i2
        end type

        type (seq1), pointer :: s1

        s1 => x

        print *, s1

        s1%i1 = 2 * s1%i2
    end subroutine
end module

program fArg029a3
use m
    type seq1
        sequence

        integer*4 i1
        integer*2 i2
    end type

    type (seq1), pointer :: s1

    s1 => replicateAnything (seq1 (2, 4))

    if ((s1%i1 /= 2) .or. (s1%i2 /= 4_2)) error stop 1_4

    call testSeq (s1)

    if ((s1%i1 /= 8) .or. (s1%i2 /= 4_2)) error stop 2_4

end
