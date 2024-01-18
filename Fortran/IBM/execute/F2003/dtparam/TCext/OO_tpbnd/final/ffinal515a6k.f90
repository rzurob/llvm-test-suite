! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal515a6k
!*
!*  DATE                       : 2007-11-11 (original: 02/15/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (C734 and C738, in FORALL header or
!                               FORALL body, any reference to the final sub must
!                               be pure)
!*
!*  KEYWORD(S)                 :
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
        integer(kbase_1), pointer :: id => null()

        contains

        final :: finalizeBase
    end type


    interface assignment(=)
        elemental subroutine copyInt (b1, id)
        import base
            class (base(4)), intent(out) :: b1 ! tcx: (4)
            integer(4), intent(in) :: id
        end subroutine

        elemental subroutine copyB2 (b1, b2)
        import base
            class (base(4)), intent(out) :: b1 ! tcx: (4)
            class (base(4)), intent(in) :: b2 ! tcx: (4)
        end subroutine
    end interface

    contains


    elemental subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        if (associated (b%id)) deallocate (b%id)
    end subroutine

    elemental logical function baseEqual (b1, b2)
        type (base(4)), intent (in) :: b1, b2 ! tcx: (4)

        if (associated (b1%id)) then
            baseEqual = associated (b2%id)

            if (baseEqual) baseEqual = (b1%id == b2%id)
        else
            baseEqual = .not. associated (b2%id)
        end if
    end function
end module

program ffinal515a6k
use m
    class (base(4)), allocatable :: b1(:) ! tcx: (4)

    allocate (b1(10))

    !! assign the 1st 5 element
    forall (i=1:5)
        b1(i) = i
    end forall

    !! assign the last 5 element
    forall (i=1:10, baseEqual(b1(i), base(4)())) ! tcx: (4)
        b1(i) = b1(11-i)
    end forall


    !! verify results
    do i = 1, 10
        if (.not. associated (b1(i)%id)) call zzrc (int(i, 4))

        if (associated (b1(i)%id, b1(11-i)%id)) call zzrc (int(10+i, 4))

        if (i <= 5 .and. (b1(i)%id /= i)) call zzrc (int (20+i, 4))
        if (i >= 6 .and. (b1(i)%id /= 11-i)) call zzrc (int (30+i, 4))
    end do
end


elemental subroutine copyInt (b1, id)
use m, only: base
    class (base(4)), intent(out) :: b1 ! tcx: (4)
    integer(4), intent(in) :: id

    allocate (b1%id, source=id)
end subroutine


elemental subroutine copyB2 (b1, b2)
use m, only: base
    class (base(4)), intent(out) :: b1 ! tcx: (4)
    class (base(4)), intent(in) :: b2 ! tcx: (4)

    if (associated (b2%id)) allocate (b1%id, source=b2%id)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 10 changes
