!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal514a16_1.f
! %VERIFY: ffinal514a16_1.out:ffinal514a16_1.vf
! %STDIN:
! %STDOUT: ffinal514a16_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function results in DO WHILE loop)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4, pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    interface assignment (=)
        subroutine base2Base (b1, b2)
        import base
            type (base), intent(out) :: b1
            type (base), intent(in) :: b2
        end subroutine
    end interface

    interface operator (==)
        logical function baseEqual (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine

    type (base) function makeBase (i)
        integer*4, optional, intent(in) :: i

        if (present (i)) then
            allocate (makeBase%data)

            makeBase%data = i
        end if
    end function
end module

program ffinal514a16_1
use m
    type (base) :: b1(4)

    i = 1

    !! the do-while loop is not doing a lazy evaluation
    do while (i <= 3 .and. b1(i) == makeBase ())
        print *, i

        b1(i) = makeBase (i=i)

        print *, 'after assignment'
        i = i + 1
    end do

    if ((b1(1)%data /= 1) .or. (b1(2)%data /= 2) .or. (b1(3)%data /=3)) &
            error stop 1_4

    print *, 'end'
end

subroutine base2Base (b1, b2)
use m, only: base
    type (base), intent(out) :: b1
    type (base), intent(in) :: b2

    if (associated (b2%data)) then
        allocate (b1%data)
        b1%data = b2%data
    end if
end subroutine

logical function baseEqual (b1, b2)
use m, only: base
    type (base), intent(in) :: b1, b2

    if ((.not. associated (b1%data)) .and. (.not. associated (b2%data))) then
        baseEqual = .true.
    else if (associated (b1%data) .and. associated (b2%data)) then
        if (b1%data == b2%data) then
            baseEqual = .true.
        else
            baseEqual = .false.
        end if
    else
        baseEqual = .false.
    end if
end function
