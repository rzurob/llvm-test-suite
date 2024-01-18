! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               global entities declared in module,
!*                               have public attribute, and are of type
!*                               integer
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
   class(*), public, allocatable :: old, new
end module

program main
use m
    allocate(integer ::old)

    if ( .not. allocated(old) ) stop 11

    select type(x => old)
        type is (integer)
            x = 33
        class default
            stop 12
    end select

    allocate(new, source = 10.8)

    if ( .not. allocated(new) ) stop 21

    call move_alloc(old, new)

    if ( .not. allocated(new) ) stop 31

    if ( allocated(old) ) stop 41

    select type(new)
        type is (integer)
            if ( new /= 33) stop 22
        class default
            stop 23
    end select
end
