! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C490: proc target used for the data
!                               pointer component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        integer(k), pointer :: id(:)
    end type
end module

program dtparamConstr020d
use m
    abstract interface
        integer(8) function genPtr8 (i)
            integer(8), intent(in) :: i(:)
            pointer genPtr8(:)
        end function

        integer(4) function genPtr4 (i)
            integer(4), intent(in) :: i(:)
            pointer genPtr4(:)
        end function
    end interface

    procedure(genPtr8), pointer :: genPtrArray8

    procedure(genPtr4), pointer :: genPtrArray4

    type (base(4,35)) :: b1

    type(base(8, :)), allocatable :: c1

    allocate (base(8,99) :: c1)

    b1 = base(4,35)(1.0, genPtrArray4)

    c1 = base(8,99)((/(i*1.0d0, i=1,99)/), genPtrArray8)
end
