! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/15/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 311904.2)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine test1 (s)
        character(*), intent(inout) :: s

        if (len(s) >= 25) then
            call test2 (s)
        else
            call test3(s)
        end if
    end subroutine

    subroutine test2 (s)
        character(5), intent(inout) :: s(5)

        s = (/character(5) :: 'POINT', 'COMMA', 'UNDEFINED', '',''/)
    end subroutine

    subroutine test3 (s)
        character(5), intent(inout) :: s(*)

        s(1:3) = (/'POINT', 'COMMA', 'UNDEF'/)
    end subroutine
end module

use m
    character(20), pointer :: s1
    character(30), allocatable :: s2, s3

    character(:), allocatable :: s4
    character(:), pointer :: s5

    allocate (s1, s2, s3)

    allocate (character(40) :: s4)
    allocate (character(16) :: s5)

    s1 = ''
    s2 = ''
    s3 = s2
    s4 = s1//repeat(' ',20)
    s5 = s3

    !! call test3 directly using character pointer
    call test3(s1)

    !! same as to call test2(s2)
    call test1(s2)

    !! call test2 directly using character allocatable
    call test2(s3)

    !! same as to call test2(s4)
    call test1(s4)

    call test3(s5)


    !! verify that s1 through s5 are all of the same values
    if (s1 /= 'POINTCOMMAUNDEF') error stop 1_4
    if (s2 /= 'POINTCOMMAUNDEF') error stop 2_4
    if (s3 /= 'POINTCOMMAUNDEF') error stop 3_4
    if (s4 /= 'POINTCOMMAUNDEF') error stop 4_4
    if (s5 /= 'POINTCOMMAUNDEF') error stop 5_4

    end
