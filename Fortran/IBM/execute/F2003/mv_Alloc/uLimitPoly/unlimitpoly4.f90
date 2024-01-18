! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               keyword FROM/TO provided in a reference to  MOVE_ALLOC
!*                               FROM/TO is dummy arg
!*                               FROM/TO has intent, save attribute
!*                               type LOGICAL
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

    class(*), allocatable :: old1, old2

    contains

        subroutine sub( arg )

            class(*), intent(inout), allocatable :: arg
            class(*), save, allocatable :: new

            if ( allocated(new) ) then
                call move_alloc(new, arg)
                if ( allocated(new) ) stop 9
            else
                if ( allocated(arg) ) stop 10

                allocate(new, source=.true.)
                allocate(arg, source= .false.)

                call move_alloc(TO=new, FROM=arg)

                if ( allocated(arg) ) stop 11
                if ( .not. allocated(new) ) stop 12
            endif

        end subroutine
end module

program main
use m

    call sub (old1)

    call sub (old2)

    if ( .not. allocated(old2) ) stop 21

    select type(old2)
        type is (logical)
            if ( old2 .neqv. .false.) stop 22
        class default
            stop 23
    end select
end
