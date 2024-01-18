!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters:
!                               defined/undefined during argument association;
!                               use allocatable scalar entities.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (l)
        integer, len :: l

        character(l) :: name = 'default'
        integer :: ids((l+1)/5) = -1
    end type

    contains

    subroutine resetBaseAlloc (b)
        type (base(:)), allocatable, intent(out) :: b
    end subroutine

    subroutine printBaseAlloc (b)
        type (base(:)), allocatable, intent(in) :: b

        if (.not. allocated(b)) then
            print *, 'input data not allocated'
        else
            print *, b
        end if
    end subroutine

    subroutine reallocBase (b, name, i1)
        type (base(:)), allocatable, intent(inout) :: b
        character(*), intent(in) :: name
        integer, intent(in) :: i1((len(name)+1)/5)

        call resetBaseAlloc (b)

        allocate(base(len(name)):: b)

        b%name = name
        b%ids = i1
    end subroutine
end module

program deferdparamDTSpec015
use m
    type (base(:)), allocatable :: b1

    call printBaseAlloc (b1)

    allocate (base(20):: b1)

    b1%name = 'xlftest b1'
    b1%ids = (/(i, i = 1, 4)/)

    call printBaseAlloc (b1)

    call reallocBase (b1, 'this is a reallocate call', (/(i*100, i=1, 5)/))

    call printBaseAlloc (b1)

    call resetBaseAlloc (b1)

    call reallocBase (b1, 'another reallocate', (/(i*10, i=1, 10)/))

    call printBaseAlloc (b1)
end
