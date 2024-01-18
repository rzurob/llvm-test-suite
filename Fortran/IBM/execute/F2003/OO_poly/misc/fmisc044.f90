!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/31/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 293792)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer, parameter :: kind_32 = 4
    integer, protected :: length1 = 100

    interface
        character(length1) function f1 (i1) !<-- compiler compilains about length1
        import
            integer(kind_32) i1
        end function
    end interface

    contains

    subroutine updateLength (l1)
        integer, intent(in) :: l1

        length1 = l1
    end subroutine
end module

program fmisc044
use m
    call printString (f1(100_4))

    call updateLength (48)

    call printString (f1 (10_4))

    contains

    subroutine printString (l)
        character(*), intent(in) :: l

        print *, l
    end subroutine
end

character(length1) function f1(i1)
use m, only: length1
    integer(4) i1

    write (f1, *) 'test case /tstdev/OO_poly/misc/fmisc044.f ;', i1
end function
