!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/15/2005
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
    type base
        integer(4), pointer :: id => null()

        contains

        final :: finalizeBase
    end type


    interface assignment(=)
        elemental subroutine copyInt (b1, id)
        import base
            class (base), intent(out) :: b1
            integer(4), intent(in) :: id
        end subroutine

        elemental subroutine copyB2 (b1, b2)
        import base
            class (base), intent(out) :: b1
            class (base), intent(in) :: b2
        end subroutine
    end interface

    contains


    elemental subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated (b%id)) deallocate (b%id)
    end subroutine

    elemental logical function baseEqual (b1, b2)
        type (base), intent (in) :: b1, b2

        if (associated (b1%id)) then
            baseEqual = associated (b2%id)

            if (baseEqual) baseEqual = (b1%id == b2%id)
        else
            baseEqual = .not. associated (b2%id)
        end if
    end function
end module

program ffinal515a6
use m
    class (base), allocatable :: b1(:)

    allocate (b1(10))

    !! assign the 1st 5 element
    forall (i=1:5)
        b1(i) = i
    end forall

    !! assign the last 5 element
    forall (i=1:10, baseEqual(b1(i), base()))
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
    class (base), intent(out) :: b1
    integer(4), intent(in) :: id

    allocate (b1%id, source=id)
end subroutine


elemental subroutine copyB2 (b1, b2)
use m, only: base
    class (base), intent(out) :: b1
    class (base), intent(in) :: b2

    if (associated (b2%id)) allocate (b1%id, source=b2%id)
end subroutine
