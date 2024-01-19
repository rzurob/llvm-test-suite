! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (select type construct syntax error
!                               should be diagnosed)
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
    type, abstract :: base
    end type

    type, extends(base) :: child
        integer(4) id
        character(21) :: name
    end type

    contains

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (.not. allocated(x)) error stop 1_4

        print *, 'bounds:', lbound(x), ubound(x)

        select type (x)
            type is (child)
                do i = lbound(x,1), ubound(x,1)
                    write (*, '(i5,a,a)', advance='no') x(i)%id, ' ', x(i)%name
                end do
            class default
                error stop 2_4
        !end select !<-- diagnose this
    end subroutine
end module

program falloc006a9d
end
