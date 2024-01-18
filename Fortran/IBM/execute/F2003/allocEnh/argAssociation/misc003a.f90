!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (actual-arg being a function
!                               result that does uses allocatable dummy-arg in
!                               the intrinsic assignment for deferred character
!                               type).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: c, c2

    allocate (character(10) :: c2)
    allocate (character(199) :: c)

    do i = 1, 10
        c2(i:i) = achar(i-1)
    end do

    call assgn (c, genStr(c, c2))

    if (c%len /= 10) error stop 1_4

    if (c /= 'FORTRAN') error stop 2_4

    contains

    character(:) function genStr (s1, s2)
        character(:), allocatable :: s1
        character(*) s2

        allocatable genStr

        s1 = max(s2, ' ')

        genStr = s1
    end function

    subroutine assgn (s1, s2)
        character(:), allocatable :: s1
        character(*) s2

        s1 = max (s2, 'FORTRAN')
    end subroutine
    end
