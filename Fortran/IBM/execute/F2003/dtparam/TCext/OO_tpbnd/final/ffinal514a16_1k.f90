! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a16_1k
!*
!*  DATE                       : 2007-11-07 (original: 04/20/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    interface assignment (=)
        subroutine base2Base (b1, b2)
        import base
            type (base(4)), intent(out) :: b1 ! tcx: (4)
            type (base(4)), intent(in) :: b2 ! tcx: (4)
        end subroutine
    end interface

    interface operator (==)
        logical function baseEqual (b1, b2)
        import base
            type (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine

    type (base(4)) function makeBase (i) ! tcx: (4)
        integer*4, optional, intent(in) :: i

        if (present (i)) then
            allocate (makeBase%data)

            makeBase%data = i
        end if
    end function
end module

program ffinal514a16_1k
use m
    type (base(4)) :: b1(4) ! tcx: (4)

    i = 1

    !! the do-while loop is not doing a lazy evaluation
    do while (i <= 3 .and. b1(i) == makeBase ())
        print *, i

        b1(i) = makeBase (i=i)

        print *, 'after assignment'
        i = i + 1
    end do

    if ((b1(1)%data /= 1) .or. (b1(2)%data /= 2) .or. (b1(3)%data /=3)) &
            error stop 101_4

    print *, 'end'
end

subroutine base2Base (b1, b2)
use m, only: base
    type (base(4)), intent(out) :: b1 ! tcx: (4)
    type (base(4)), intent(in) :: b2 ! tcx: (4)

    if (associated (b2%data)) then
        allocate (b1%data)
        b1%data = b2%data
    end if
end subroutine

logical function baseEqual (b1, b2)
use m, only: base
    type (base(4)), intent(in) :: b1, b2 ! tcx: (4)

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


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 9 changes
