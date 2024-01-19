! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/02/2006
!*
!*  DESCRIPTION                : dtparam (section 16.4.1.2: host association)
!                               Case: Name conflict between type parameters and
!                               entities in the host: host entities are not
!                               accessible.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamScope001d
    integer, parameter :: k = 8

    type k
    end type

    abstract interface
        subroutine test()
        end subroutine
    end interface

    type A(k)
        integer, kind :: k = k  !<-- illegal, k is inaccessible

        real(k) data
    end type

    type B(k)
        integer, kind :: k

        class(k), pointer :: p => null() !<-- illegal: type(k) is inaccessible
    end type

    type C (abs)
        integer, len :: abs

        procedure(abs), pointer, nopass :: p !<-- illegal: intrinsic abs inaccessible
    end type

    type D (test)
        integer, len :: test

        procedure(test), pointer, nopass :: p !<-- illegal: abstract interface test inaccessible
    end type
    end
