!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Mis-use of a function call that returns a
!                               procedure pointer as a data object. (diagnostic
!                               case)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type procPtr (k)
        integer, kind :: k

        procedure(real(k)), pointer, nopass :: proc
    end type

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        type(procPtr(k)) :: x(n)
        procedure(select), pointer :: select
    end type

    abstract interface
        function select (b1, n)
        import
            class(base(4,*)), intent(in) :: b1
            integer, intent(in) :: n

            procedure(real(4)), pointer :: select
        end function
    end interface
end module

program dtparamConstr038d
use m
    real(4), pointer :: r1

    type (base(4, 100)) :: b1

    b1 = base(4,100)(1.0, procPtr(4)(r1), null())

    r1 => b1%select (10)
end
