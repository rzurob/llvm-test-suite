! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet015.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2005
!*
!*  DESCRIPTION                : poly function return (used as the actual arg to
!                               be associated with intent(in) dummy-pointer-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind            :: k1
        integer(k1), allocatable :: ids(:)

        contains

        procedure :: print => printBase
        procedure :: makeData => makeBaseAlloc
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind                      :: k2
        integer, len                       :: n1
        character(kind=k2,len=n1), pointer :: names(:) => null()

        contains

        procedure :: print => printChild
        procedure, pass(c) :: assgnName => allocateNames
        procedure :: makeData => makeChildAlloc
        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child(8,1,*)), intent(inout) :: c

        if (associated (c%names)) then
            print *, 'deallocating names'

            deallocate (c%names)
        end if
    end subroutine

    subroutine allocateNames (names, c)
        class (child(8,1,*)), intent(inout) :: c
        character(*), intent(in) :: names(:)

        if (associated (c%names)) deallocate (c%names)

        allocate (c%names(size(names)), source=names)
    end subroutine

    subroutine printBase (b)
        class(base(8)), intent(in) :: b

        if (allocated (b%ids)) then
            print *, b%ids
        end if
    end subroutine

    subroutine printChild (b)
        class(child(8,1,*)), intent(in) :: b

        call b%base%print

        if (associated(b%names)) then
            print *, b%names
        end if
    end subroutine

    class (base(8)) function makeBaseAlloc (b)
        allocatable makeBaseAlloc
        class (base(8)), intent(in) :: b

        allocate (makeBaseAlloc, source=b)
    end function

    class (base(8)) function makeChildAlloc (b)
        allocatable makeChildAlloc
        class (child(8,1,*)), intent(in) :: b

        allocate (makeChildAlloc, source=child(8,1,20)(b%ids))

        select type (makeChildAlloc)
            type is (child(8,1,*))
                call makeChildAlloc%assgnName(b%names)
            class default
                error stop 10_4
        end select
    end function

    subroutine printData (d)
        class (base(8)), allocatable, intent(in) :: d

        call d%print
    end subroutine
end module

program ffuncRet015
use m
    class (base(8)), pointer     :: b1
    class (base(8)), allocatable :: b2

    allocate (b1, source=child(8,1,20)((/5,4,3,2,1/)))

    select type (b1)
        type is (child(8,1,*))
            call b1%assgnName ((/'test 5', 'test 4', 'test 3', 'test 2', &
                        'test 1'/))
        class default
            error stop 1_4
    end select

    allocate (b2, source=b1%makeData())
    if (allocated (b2)) then
        call printData (b2)
    end if

    deallocate (b1)

    print *, 'end'
end
