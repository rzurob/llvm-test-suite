! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is poly type of child
!*                               Pointer is poly type of base with final sub
!*                               TO is unlimited poly
!*                               defect 322595
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
        integer id
        contains
            final :: finalA
   end type

   contains
       subroutine finalA(a)
            type( base), intent(inout) :: a(:)
       end subroutine
end module

use m

    class(*), target,  allocatable :: to(:)
    class(base), pointer :: p(:)

    type, extends(base) :: A
       class(base), allocatable :: x
    end type

    class(A), target, allocatable :: from(:)

    allocate(from(3), source = (/ (A( x=base(i+10), id=i), i=1,3) /) )

    p => from

    call move_alloc(from, to)

    if ( allocated(from) ) error stop 11
    if ( .not. allocated(to)) error stop 13

    select type (to)
        type is (A)
            if ( .not. associated(p, to) ) error stop 23

 	    do i = 1, 3
                if ( .not. allocated(to(i)%x) )  call zzrc(i)
            end do
            if ( to(1)%x%id /= 11 ) error stop 31
            if ( to(2)%x%id /= 12 ) error stop 33
            if ( to(3)%x%id /= 13 ) error stop 35
        class default
            stop 41
    end select
end
