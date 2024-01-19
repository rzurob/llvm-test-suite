! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (procedure as the actual
!*                               argument; used for arrays)
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
        integer*4 :: id = -1
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'
    end type

    contains

    subroutine add (b, i)
        class (base), intent(inout) :: b(:)
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine

    subroutine subtract (b, i)
        class (base), intent(inout) :: b(:)
        integer*4, intent(in) :: i

        b%id = b%id - i
    end subroutine
end module

module m1
use m
    contains

    subroutine modify (b, op, i)
        class (base), intent(inout) :: b(:)
        integer*4, intent(in) :: i

        interface
            subroutine op (b1, i1)
            use m
                class (base), intent(inout) :: b1(:)
                integer*4, intent(in) :: i1
            end subroutine
        end interface

        call op (b, i)
    end subroutine
end module

program fArg001a3
use m1
    interface
        subroutine op1 (b, i)
        use m
            class (base), intent(inout) :: b(:)
            integer*4, intent(in) :: i
        end subroutine
    end interface

    type (base) :: b1 (2:5)
    type (child) :: c1 (3:5)

    c1%name = 'c1'

    call modify (b1, add, 2)

    call modify (c1, subtract, 9)

    if (any (b1%id /= 1)) error stop 1_4

    if (any (c1%id /= -10) .or. any (c1%name /= 'c1')) error stop 2_4

    call modify (c1, op=op1, i=10)

    if (any (c1%id /= (/1,2,3/))) error stop 3_4

    if (any (c1%name /= 'c1')) error stop 4_4

    call modify (b1(3::2), op = add, i = 1)

    print *, b1

    call modify (c1(3:4)%base, op1, i=8)

    print *, c1
end

subroutine op1 (b, i)
use m
    class (base), intent(inout) :: b(:)
    integer*4, intent(in) :: i

    do ind = 1, size(b)
        b(ind)%id = b(ind)%id + i + ind
    end do
end subroutine
