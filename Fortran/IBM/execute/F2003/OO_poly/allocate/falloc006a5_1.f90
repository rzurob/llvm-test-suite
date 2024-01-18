! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocate unlimited poly allocatable
!                               array with type-spec of derived type)
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
        integer, pointer :: id => null()
    end type

    type, extends(base) :: child
        character(18) :: name = 'default'
    end type

    contains

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (.not. allocated(x)) error stop 1_4

        print *, 'bounds:', lbound(x,1), ubound(x, 1)

        select type (x)
            class is (base)
                call printBaseArray (x)
            class default
                error stop 2_4
        end select
    end subroutine


    subroutine printBaseArray (b1)
        class (base), intent(in) :: b1(:)

        do i = 1, size (b1)
            if (associated (b1(i)%id)) then
                write (*, '(i5)', advance='no') b1(i)%id
            else
                write (*, '(a)', advance='no') 'id not associated'
            end if

            select type (x => b1(i))
                type is (base)
                    print *, ''
                type is (child)
                    print *, ', ', x%name
                class default
                    error stop 5_4
            end select
        end do
    end subroutine

    subroutine createX (x, src)
        class(*), intent(out), allocatable :: x(:)
        class (*), intent(in) :: src(:)

        select type (src)
            type is (base)
                allocate (base :: x(size(src)))
            type is (child)
                allocate (child :: x(size(src)))
            class default
                error stop 10_4
        end select
    end subroutine
end module


program falloc006a5_1
use m
    class (*), allocatable :: x1(:)


    call createX (x1, (/(child (null(), 'abc'), i=1,3)/))

    call printX (x1)

    call createX (x1, (/base()/))

    call printX (x1)
end
