! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg001a3.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'
    end type

    contains

    subroutine add (b, i)
        class (base(4)), intent(inout) :: b(:)
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine

    subroutine subtract (b, i)
        class (base(4)), intent(inout) :: b(:)
        integer*4, intent(in) :: i

        b%id = b%id - i
    end subroutine
end module

module m1
use m
    contains

    subroutine modify (b, op, i)
        class (base(4)), intent(inout) :: b(:)
        integer*4, intent(in) :: i

        interface
            subroutine op (b1, i1)
            use m
                class (base(4)), intent(inout) :: b1(:)
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
            class (base(4)), intent(inout) :: b(:)
            integer*4, intent(in) :: i
        end subroutine
    end interface

    type (base(4)) :: b1 (2:5)
    type (child(4,1,20)) :: c1 (3:5)

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
    class (base(4)), intent(inout) :: b(:)
    integer*4, intent(in) :: i

    do ind = 1, size(b)
        b(ind)%id = b(ind)%id + i + ind
    end do
end subroutine
