! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/1/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 327536)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    class(*), allocatable :: x

    character(:), allocatable :: c

    c = 'abcd efg'

    allocate (x, source=c)

    select type (x)
        type is (character(*))
            if (len(x) /= 8) error stop 5

            if (x%len /= 8) error stop 10

    end select
    end