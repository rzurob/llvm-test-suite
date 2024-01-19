! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/11/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: can NOT be used for
!                               entities or components without
!                               pointer/allocatable attribute. (C403)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program deferdparamDTSpec001d
    type base (n)
        integer, len :: n

        integer ids(n)
    end type

    type(base(:)) :: b1     !<-- illegal

    type base1(l)
        integer, len :: l

        character(l) :: name
        type(base(:)) b1    !<-- illegal
    end type
end
