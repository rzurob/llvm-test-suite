!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C481, part 2: The assumed-type-parameter
!                               in type-guard statement in select type
!                               construct.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n)
        integer, len :: n

        integer :: ids(n) = -1
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: names(n) = 'default'
    end type

    contains

    subroutine printX (x)
        class(*), intent(in) :: x(:)

        select type (x)
            class is (base(n=*))
                do i = 1, size(x)
                    print *, x(i)%ids
                end do

            class is (child(l=*, n=*))
                do i = 1, size(x)
                    do j = 1, x%n
                        print *, x(i)%ids(j), x(i)%names(j)
                    end do
                end do

            class default
                print *, 'unexpected type'
        end select
    end subroutine
end module

program dtSpec008
use m
    class(*), allocatable :: x(:)
    class(base(:)), pointer :: b1(:)

    allocate (child(n=10,l=20):: b1(3))

    allocate (x(5), source=(/(base(4)((/(j, j=i,i+3)/)), i=1,5)/))

    select type (x => b1)
        class is (child(n=*,l=*))
            do i = 1, 3
                x(i)%ids = (/(i*100 + j, j=1, x%n)/)
                x(i)%names = 'xlftest'
            end do
        class default
            error stop 10_4
    end select

    call printX (b1)
    call printX (x)
end
