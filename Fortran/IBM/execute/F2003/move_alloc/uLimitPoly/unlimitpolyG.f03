! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1.FROM and TO are unlimited polymorphic,
!*                               FROM and TO are rank two component of the
!*                               2.same derived-type
!*                               3.TO is finalized , FROM is not finalized
!*                               4.check dynamic type, size, value
!*                               5.FROM and TO are accesed by associate name of
!*                                      select type
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type parent
         character(8) :: name
         contains
            final :: final1
    end type

    integer, save :: countP = 0
    integer :: countC = 0

    type, extends(parent) :: child
	contains
	    final :: final2
    end type

    contains
        subroutine final1(arg)
            type(parent), intent(in)  :: arg(:,:)
            countP = countp + 1
        end subroutine
        subroutine final2(arg)
            type(child), intent(inout)  :: arg(:,:)
            countC = countC + 1
        end subroutine
end module


program main
use m

    integer i

    type level1
        class (*), allocatable :: from(:,:)
        class (*), allocatable :: to(:,:)
    end type

    type level2
        class(*), allocatable ::  l2
    end type

    type(level2) :: ll

    allocate( level1::ll%l2 )
    if ( .not. allocated(ll%l2) ) error stop 21

    select type ( x => ll%l2 )
        class is (level1)
            allocate( x%from(1,1), source = child("FORTRAN"))
            if ( .not. allocated(x%from) ) error stop 31

            allocate( x%to(2,2), source = reshape( (/ (parent("COMPILER"), i = 1,4) /), (/2, 2/) ) )
            if ( .not. allocated(x%to) ) error stop 32

            countP = 0
            countC = 0

            call move_alloc(x%from, x%to)

            if ( .not. allocated(x%to) ) error stop 35
            if ( allocated(x%from) ) error stop 37

            if ( size(x%to, 1) /= 1) error stop 41
            if ( size(x%to, 2) /= 1) error stop 42
            if ( countp /= 1) error stop 43
	    if ( countC /= 0) error stop 44

            select type ( y => x%to)
                type is (child)
                   if ( y(1,1)%name /= "FORTRAN" ) error stop 51
                class default
                   print *, "wrong type"
                   stop 53
            end select

        class default
            print *, "wrong type"
            stop 41
    end select


   end
