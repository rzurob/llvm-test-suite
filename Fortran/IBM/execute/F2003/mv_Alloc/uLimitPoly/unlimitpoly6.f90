! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/25/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               dummy args with volatile, optional,
!*                               intent attributes
!*
!*                               The intrinsic is referenced in external
!*                               procedure
!*                               dynamic type is type character
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

   class(*), allocatable :: from

   interface
        subroutine sub(from,to)
            class(*), volatile, allocatable :: from
            class(*), optional, allocatable :: to
        end subroutine
   end interface
end module

program main
use m

   class(*), allocatable :: to

   call sub(from, to)

   if ( .not. allocated(to) ) error stop 21

   select type(to)
       type is (character(*))
            if ( to /= "helloworld" ) error stop 23
       class default
            stop 22
   end select

end

subroutine sub(from,to)
    class(*), volatile, allocatable :: from
    class(*), optional, intent(inout), allocatable :: to

    allocate(from, source="helloworld")
    if ( .not. allocated(from) ) error stop 11

    if (present(to)) then
        call move_alloc(from, to)
        if ( allocated(from)) error stop 12
    else
        stop 13
    endif
end subroutine
