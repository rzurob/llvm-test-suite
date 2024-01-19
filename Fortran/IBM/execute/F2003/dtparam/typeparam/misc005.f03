! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/13/2006
!*
!*  DESCRIPTION                : miscellaneous (compiler ICE)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base! (l)
        character, allocatable :: name(:)
        integer, allocatable :: ids(:)
    end type

    contains

    subroutine resetBaseAlloc (b)
        type (base), allocatable, intent(out) :: b
    end subroutine

    subroutine printBaseAlloc (b)
        type (base), allocatable, intent(in) :: b

        if (.not. allocated(b)) then
            print *, 'input data not allocated'
        else
            print *, b%name, b%ids
        end if
    end subroutine

    subroutine reallocBase (b, name, i1)
        implicit none
        type (base), allocatable, intent(inout) :: b
        character(*), intent(in) :: name
        integer, intent(in) :: i1((len(name)+1)/5)
        integer i

        call resetBaseAlloc (b)

        allocate(base:: b)

        !The following line uses an ACIMPDO and it seg-faults at runtime
!        allocate (b%name(len(name)), source=(/(name(i:i), i=1, len(name))/))

        ! this is a rewrite of the code that preserves the semnatics but by pass
        ! the ACIMPDO issue
        allocate (b%name(len(name)))

        do i = 1, len(name)
            b%name(i) = name(i:i)
        end do

!        b%name = name
        allocate (b%ids(size(i1)), source=i1)
!        b%ids = i1
    end subroutine
end module

program deferdparamDTSpec015
use m
    type (base), allocatable :: b1

    call printBaseAlloc (b1)

    allocate (base:: b1)

    !The following line produces a blank line due to ACIMPDO issue
!    allocate(b1%name(20), source = (/('xlftest b1          '(i:i), i=1, 20)/))

    ! this is a bypass of the ACIMPDO problem
    allocate(b1%name(20))

    do i = 1, 20
        b1%name(i) = 'xlftest b1          '(i:i)
    end do

    allocate (b1%ids(4), source = (/(i, i = 1, 4)/))

    call printBaseAlloc (b1)

    call reallocBase (b1, 'this is a reallocate call', (/(i*100, i=1, 5)/))

    call printBaseAlloc (b1)

    call resetBaseAlloc (b1)

    call reallocBase (b1, 'another reallocate', (/(i*10, i=1, 10)/))

    call printBaseAlloc (b1)
end
