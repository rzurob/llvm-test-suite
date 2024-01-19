! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-shape unlimited
!                               poly dummy-arg associated with array sections of
!                               sequence type)
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

program fArg603a1
    class(*), pointer :: i(:)

    interface
        subroutine print3 (x)
            class (*), intent (in) :: x(:)
        end subroutine
    end interface

    type seq1
        sequence
        integer(4) i1, i2
    end type

    integer i2(10)

    type (seq1), target :: s1 (100)

    i2 = (/1, 11, 21, 31, 41, 51, 61, 71, 81, 91/)

    s1 = (/(seq1(j, j*100), j=1,100)/)

    i => s1

    call print3(i)
    call print3(i(i2))
    call print3(i(::2))
end

subroutine print3(p)
    class(*), intent(in) :: p(:)

    type seq1
        sequence
        integer(4) i1, i2
    end type

    type (seq1), pointer :: s1(:)

    associate (x => transfer (p, s1, 3))
        print *, x
    end associate
end subroutine
