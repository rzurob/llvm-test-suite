! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimited polymorphic,
!*                               rank 1
!*				 FROM is local var, TO is component of derive-type
!*                               TO is finalized
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer id

        contains
            final :: finalA
    end type

    contains
       subroutine finalA(arg)
            type(A), intent(in)  :: arg
            print *, "this is final procedure for A, id = " , arg%id
        end subroutine
end module


program main
use m

    integer i
    class (*), allocatable :: x(:)

    type base
        class (*), allocatable :: value(:)
    end type

    class(base), allocatable :: b1

    allocate(x(5), source = (/ ( A(i), i = 1, 5 ) /) )
    allocate(b1, source = base(x) )

    deallocate(x)

    allocate (x(2), source= (/ (1.1d2, 2.5d1), (4.8d2, 9.6d1) /) )

    call move_alloc( x , b1%value )

    if ( allocated(x) ) stop 41
    if ( .not. allocated(b1%value) ) stop  51

    select type( p => b1%value)
        type is (complex(8))
            write (*, '(2d12.3)')  p
        class default
           stop 33
   end select
   end

