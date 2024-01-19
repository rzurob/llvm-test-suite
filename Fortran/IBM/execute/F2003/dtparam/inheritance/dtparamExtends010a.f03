! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/30/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statements: An extended type includes all of the
!                               type parameters of its parent.  Additional type
!                               parameters may be declared in the definition of
!                               the extended type.  And private parent
!                               components.
!
!                               Case: private components' name can be used as a
!                               type-parameter name outside the module.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract, private :: k2(kind, len)
        integer, kind :: kind
        integer, len :: len

        logical(kind) flag
    end type

    type, extends(k2) :: base
        integer(kind) id
        character(len) name
    end type
end module

module m1
use m
    type, extends(base) :: child (k2)   !<-- legal to use k2 as new type-param
        integer, kind :: k2

        real(max(kind,k2)) data (len)
    end type

    type (child(4, 20, 8)) c1_m
end module


program dtparamExtends010a
use m1
    type (child(kind=8, len=20, k2=4)) c1

    c1_m%flag = .true.
    c1%flag = c1_m%flag

    c1_m%name = 'c1_m module variable'
    c1%name = 'c1 local variable in main'

    c1_m%id = 1
    c1%id = 2*c1_m%id

    c1_m%data = (/(i*1.2d0, i = 1, 20)/)
    c1%data = c1_m%data * 2.0d0


    !! verify the data components

    call printScalarX(c1_m%flag)
    call printScalarX(c1_m%id)
    call printScalarX(c1_m%name)
    call printArrayX (c1_m%data, 20)


    call printScalarX(c1%flag)
    call printScalarX(c1%id)
    call printScalarX(c1%name)
    call printArrayX (c1%data, 20)

    contains

    subroutine printScalarX (x)
        class(*), intent(in) :: x

        select type (x)
            type is (integer(4))
                print *, 'int4:', x
            type is (integer(8))
                print *, 'int8:', x
            type is (character(*))
                print *, 'char:', x
            type is (logical(4))
                print *, 'log4:', x
            type is (logical(8))
                print *, 'log8:', x
            type is (real(4))
                write (*, '(a,1x,f10.2)') 'real4:', x
            type is (real(8))
                write (*, '(a,1x,f12.2)') 'real8:', x
            class default
                error stop 10_4
        end select
    end subroutine

    subroutine printArrayX (x, n)
        class(*), intent(in) :: x(n)
        integer, intent(in) :: n

        do i = 1, n
            call printScalarX (x(i))
        end do
    end subroutine
end
