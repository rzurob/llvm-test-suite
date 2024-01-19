! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2006
!*
!*  DESCRIPTION                : miscellanous (defect 325904, 2nd test case)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: c(:)

    c = ['a', 'b', 'c']

    call t (int(c(1)%len))
    call t (int(c%len))

    call t(len(c(1)))
    call t(len(c))

    contains

    subroutine t (i)
        if (i /= 1) error stop 10
    end subroutine
    end
