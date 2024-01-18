!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc006a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (unlimited poly allocatable scalars as
!                               the allocate-objects; allocate statement with
!                               source-expr)
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

program falloc006a1
    type base
        integer, allocatable :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    class (*), allocatable :: x1

    !! test the integer type
    allocate (x1, source=100_8)

    select type (x1)
        type is (integer(8))
            if (x1 /= 100_8) error stop 1_4
        class default
            error stop 2_4
    end select

    deallocate (x1)

    !! test the derived type
    allocate (x1, source=base(10))

    select type (x1)
        type is (base)
            if (.not. allocated(x1%id)) error stop 2_4
            if (x1%id /= 10) error stop 3_4
        class default
            error stop 4_4
    end select

    !! use of child type
    deallocate (x1)

    allocate (x1, source=child(1, 'xlftest'))

    select type (x1)
        class is (base)
            if (.not. allocated (x1%id)) error stop 5_4
            if (x1%id /= 1) error stop 6_4

            select type (x1)
                type is (child)
                    if (x1%name /= 'xlftest') error stop 7_4
                class default
                    error stop 8_4
            end select

        class default
            error stop 9_4
    end select
end
