! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2005
!*
!*  DESCRIPTION                : miscellanous items (defect 294679)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc024a
    integer i1
    common /cb/ i1

    interface
        subroutine xyz ()
        end subroutine
    end interface

    if (i1 /= 5) error stop 1_4

    call xyz()

    if (i1 /= 10) error stop 2_4
end

subroutine xyz()
    integer i1
    common /cb/ i1

    associate (x => i1)
        x = 10
    end associate
end subroutine

block data
    integer i1
    common /cb/ i1
    data i1 /5/
end block data

