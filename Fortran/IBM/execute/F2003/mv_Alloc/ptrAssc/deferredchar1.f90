! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of
!*                                  - type character(:)
!*                                  - component of DT
!*                               after pnt is associated with from, from is
!*                               redefined by = to pnt
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    character(:, kind=1), pointer :: p1(:)
    character(len=:), allocatable, target :: from(:)
end module

program main
use m
    type ::  base
        character(:), allocatable :: to (:)
        character(:), allocatable :: from (:)
    end type

    class(base), allocatable, target :: a

    allocate(a)

    allocate(a%from(5), source = (/ '01','23','45','67','89' /))

    p1 => a%from

    allocate(a%to(10), source =  (/( char(i), i=72,81 ) /)  )

    p1(::2) = a%to(::4)

    call move_alloc(a%to, a%to)

    call move_alloc(a%from, a%to)

    if ( allocated(a%from) ) stop 11
    if ( .not. allocated(a%to) ) stop 13

    if ( .not. associated(p1, a%to)  ) stop 21
    print *,  a%to

end

