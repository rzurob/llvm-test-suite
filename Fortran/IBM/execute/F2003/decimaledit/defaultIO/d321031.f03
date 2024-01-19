! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/05/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 321031)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: c1

    allocate (c1, source='abc '// 'xyz ' //'IBM')

    print *, c1

    print *, c1(1:), len (c1(1:))

    call test1 (c1(1:))

    contains

    subroutine test1 (c)
        character(*) c

        print *, len(c)
        print *, c
    end subroutine
    end
