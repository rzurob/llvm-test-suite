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
!*  DATE                       : 01/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Test the sequence type with procedure
!                               component with PASS attribute; kind parameter.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type seq1 (k)
        integer, kind :: k

        sequence

        integer(k) :: i, j
        procedure(updateSeq4), pointer :: update4 => null()
        procedure(updateSeq8), pointer :: update8 => null()
    end type

    contains

    subroutine updateSeq4 (s, i, j)
        type (seq1(4)), intent(inout) :: s
        integer(4), intent(in) :: i, j

        s%i = i
        s%j = j
    end subroutine

    subroutine updateSeq8 (s, i, j)
        type (seq1(8)), intent(inout) :: s
        integer(8), intent(in) :: i, j

        s%i = i
        s%j = j
    end subroutine
end module

program dtparamProcComp002
use m
    integer long
    parameter (long = 8)

    type (seq1(4)), allocatable :: s1(:)
    type (seq1(long)), pointer :: s2, s3(:)

    procedure(updateSeq8) u8

    allocate (s1(10), s2, s3(10))

    s1(1)%update4 => updateSeq4

    call s1(1)%update4 (10, 100)

    s1(3:5) = s1(1)



    s2%update8 => u8
    call s2%update8 (10_8, 100_8)

    s3 = s2

    !! verify
    do i = 3, 5
        if ((s1(i)%i /= 10) .or. (s1(i)%j /= 100)) error stop 1_4
        if (.not. associated(s1(i)%update4, updateSeq4)) error stop 2_4
    end do

    do i = 1, 10
        if ((s3(i)%i /= -90) .or. (s3(i)%j /= 110)) error stop 3_4
        if (.not. associated(s3(i)%update8, u8)) error stop 4_4
    end do
end


subroutine u8 (s, i, j)
use m
    type (seq1(8)), intent(inout) :: s
    integer(8), intent(in) :: i, j

    s%i = i - j
    s%j = i + j
end subroutine
