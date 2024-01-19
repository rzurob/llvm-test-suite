! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/16/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use procedure target whose interface
!                               mismatches with pointer component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(produceBasePtr), pointer :: gen8
    end type

    abstract interface
        function produceBasePtr (b1, d1)
        import
            class(base(8,*)), intent(in) :: b1
            real(8), intent(in) :: d1(:)

            type(base(8,:)), pointer :: produceBasePtr
        end function
    end interface
end module

program dtparamConstr034d4
use m
    interface
        function genBase (b1, d1)
        use m
            class(base(4,*)), intent(in) :: b1
            real(4), intent(in) :: d1(:)

            type(base(4,:)), pointer :: genBase
        end function
    end interface

    type (base(8, 225)) :: b1

    b1 = base(8, 225)(1.0, genBase)
end
