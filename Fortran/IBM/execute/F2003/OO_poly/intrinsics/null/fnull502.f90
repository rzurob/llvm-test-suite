!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2005
!*
!*  DESCRIPTION                : NULL (assumed-length character dummy-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fnull502
    character(*), parameter :: string1 = 'xlftest case'

    character(3), pointer :: c11

    character(len(string1)), pointer :: c12

    character(len(string1(2:4))), allocatable :: c13

    allocate (c11, source='abc')

    call test1 (null(c11), len(c11))

    call test1 (null(c12), len(string1))

    call test2 (null(c13), len(string1(3:5)))

    contains

    subroutine test1 (c, length)
        character(*), pointer, intent(in) :: c
        integer, intent(in) :: length

        if (len(c) /= length) error stop 10_4

        if (associated (c)) error stop 11_4
    end subroutine

    subroutine test2 (c, length)
        character(*), allocatable, intent(in) :: c
        integer, intent(in) :: length

        if (len(c) /= length) error stop 20_4

        if (allocated (c)) error stop 21_4
    end subroutine
end
