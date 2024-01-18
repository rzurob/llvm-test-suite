!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass009a2.f
! %VERIFY: fclass009a2.out:fclass009a2.vf
! %STDIN:
! %STDOUT: fclass009a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (defined operator that returns
!                               poly-allocatable array used in associate)
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
    type base
        integer(4), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface operator(+)
        class (base) function addB1B2 (b1, b2)
        import base
            class (base), intent(in) :: b1(:), b2(:)
            allocatable :: addB1B2(:)
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id
        else
            print *, 'id not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id, b%name
        else
            print *, 'id not allocated; ', b%name
        end if
    end subroutine
end module

program fclass009a2
use m
    class (base), allocatable :: b1(:), b2(:)

    allocate (b1(2), source=(/child (1, 'xlftest'), child(10, 'team')/))
    allocate (b2(2), source=(/child (100, 'team'), child (20, 'member')/))

    associate (x => b1 + b2)
        if (size(x) /= 2) error stop 20_4

        call x(1)%print
        call x(2)%print
    end associate
end


class (base) function addB1B2 (b1, b2)
use m, only: base, child
    class (base), intent(in) :: b1(:), b2(:)
    allocatable :: addB1B2(:)

    integer id1, id2
    character(20) :: localName

    id1 = 0
    id2 = 0

    if (.not. same_type_as (b1, b2)) error stop 1_4

    if (size (b1) /= size(b2)) error stop 2_4

    select type (b1)
        type is (base)
            allocate (addB1B2(size(b1)))

            do i = 1, size(b1)
                id1 = 0
                id2 = 0

                if (allocated (b1(i)%id)) id1 = b1(i)%id
                if (allocated (b2(i)%id)) id2 = b2(i)%id

                allocate (addB1B2(i)%id, source= (id1 + id2))
            end do

        type is (child)
            allocate (child:: addB1B2(size(b1)))

            select type (b2)
                type is (child)
                    select type (addB1B2)
                        type is (child)
                            do i = 1, size(b1)
                                id1 = 0
                                id2 = 0

                                if (allocated (b1(i)%id)) id1 = b1(i)%id
                                if (allocated (b2(i)%id)) id2 = b2(i)%id

                                localName = trim(b1(i)%name) // ' ' &
                                                    // trim(b2(i)%name)

                                allocate (addB1B2(i)%id, source=id1+id2)
                                addB1B2(i)%name = localName
                            end do
                        class default
                            error stop 7_4
                    end select
                class default
                    error stop 8_4
            end select
        class default
            error stop 10_4
    end select
end function
