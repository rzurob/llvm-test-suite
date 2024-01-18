!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 311904.2)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), pointer :: mode

    character(:), allocatable :: mode2

    allocate(character(50) :: mode, mode2)

    mode = ''
    mode2 = mode

    open (10, file='test10')
    open (11, access='stream', form='formatted')
    open (12, access='direct', file='test12', recl=1000)

    call test1 (9, mode)

    call test2 (9, mode(26:))

    call test1 (9, mode2)

    call test2 (9, mode2(26:))

    !! verify the value of mode, mode2

    if (mode /= 'UNDEFPOINTPOINTUNDEF     UNDEFPOINTPOINTUNDEF') error stop 1_4
    if (mode2 /= 'UNDEFPOINTPOINTUNDEF     UNDEFPOINTPOINTUNDEF') error stop 2_4

    contains

    subroutine test1 (unit, s)
        integer, intent(in) :: unit
        character(5), intent(inout) :: s(4)

        do i = unit, unit+3
            inquire (i, decimal=s(i-unit+1))
        end do
    end subroutine

    subroutine test2 (unit, s)
        integer, intent(in) :: unit
        character(5), intent(inout) :: s(*)

        do i = unit, unit+3
            inquire (i, decimal=s(i-unit+1))
        end do
    end subroutine
end
