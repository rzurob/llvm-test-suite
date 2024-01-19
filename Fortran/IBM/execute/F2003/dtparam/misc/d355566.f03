! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/05/2009
!*
!*  DESCRIPTION                : miscellaneous (substring problem: defect
!                               355566)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: str(:)

    allocate (character(10) :: str(3))
    str(:) = 'xlftest'

    print *, str(1:3)(1:3)

    call sub (str(1:3)(1:3))

    associate (x => str(1:3)(1:3))
        print *, x
    end associate

    contains

    subroutine sub (s)
        character(*) s(:)

        print *, s
    end subroutine
end
