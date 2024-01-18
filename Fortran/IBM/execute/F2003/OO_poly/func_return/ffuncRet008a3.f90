!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/25/2005
!*
!*  DESCRIPTION                : poly-function results (result keyword use in
!                               the function definition; unlimited poly function
!                               return allocatable array)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    function makeArray (x, size) result (array)
        class(*), intent(in) :: x
        integer, intent(in) :: size

        class (*), allocatable :: array(:)

        allocate (array(size), source=x)
    end function
end module

module m1
    contains

    function verifyResult (x, s1) result (returnVal)
        class(*), intent(in), target :: x(:)

        type seq1
            sequence
            integer i, j
            real(8) r
        end type

        type(seq1), intent(in) :: s1

        logical returnVal (size(x))

        type (seq1), pointer :: s2(:)

        s2 => x

        returnVal = ( (s2%i == s1%i) .and. (s2%j == s1%j) .and. &
                        realEqual(s2%r, s1%r))
    end function

    function realEqual (r1, r2) result (returnVal)
        real(8), intent(in) :: r1(:)
        real(8), intent(in) :: r2

        logical returnVal (size(r1))
        logical(4) precision_r8

        do i = 1, size(r1)
            returnVal (i) = precision_r8(r1(i), r2)
        end do
    end function
end module

program ffuncRet008a3
use m
use m1
    type seq1
        sequence
        integer i, j
        real(8) r
    end type

    associate (x => makeArray (seq1(10, 20, 2.3_8), 1000))
        if (size(x) /= 1000) error stop 1_4

        select type (y => x)
            class default
                if (.not. all(verifyResult (y, seq1(10, 20, 2.3_8)))) error stop 2_4
        end select
    end associate
end
