! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/05/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameters shall not be used
!                               in initialization expressions: used in places of
!                               kind type param for components that of derived
!                               types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program lenparamInitexpr002d
    type base (k)
        integer, kind :: k

        integer(k) :: id
    end type

    !!!! Note: the error messages for the following errors may not be finalized
    !yet.
    type A (l)
        integer, len :: l

        class (base(l)), pointer :: p => null() !<-- illegal use of l
        procedure(type(base(l*2))), pointer, nopass :: q !<-- illegal use of l
    end type

    type (A(:)), allocatable :: a1(:)   !<-- how to diagnose this?
end
