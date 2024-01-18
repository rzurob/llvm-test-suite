!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  DESCRIPTION                : extends keyword (inheritance relation defined
!                               in module procedure and internal procedures;
!                               defect 303935)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract:: base(k1)
        integer, kind :: k1
    end type

    contains

    subroutine test1 (i)
        integer, intent(in) :: i

        type, extends(base) :: child1(k2)
            integer, kind :: k2
            integer(k2) id
        end type

        type(child1(4,8)) c1

        c1 = internal01 (i)

        if (c1%id /= i) error stop 10_4

        contains

        type(child1(4,8)) function internal01 (i)
            integer, intent(in) :: i

            type, extends(child1) :: gen3(n1)
                integer, len :: n1
                character(n1) :: name
            end type

            type(gen3(4,8,10)) g1

            g1 = gen3 (4,8,10)(i, 'xlftest')

            internal01 = g1%child1
        end function
    end subroutine
end module


program fext047
use m

    call test1 (11)

    call test2 ('long string that exceeds 20 characters long')

    contains

    subroutine test2 (c)
        character(*), intent(in) :: c

        type, extends(base) :: child2(n2)
            integer, len :: n2
            character(n2) name
        end type

        type (child2(4,20)) c1

        c1 = child2 (4,20)(c)

        print *, c1%name
    end subroutine
end
