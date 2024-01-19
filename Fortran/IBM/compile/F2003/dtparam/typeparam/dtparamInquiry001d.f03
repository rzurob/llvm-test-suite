! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/03/2006
!*
!*  DESCRIPTION                : dtparam
!                               Case: Type parameter inquiry can not appear on
!                               the LHS of an assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamInquiry001d
    type A (k, l)
        integer, kind :: k
        integer, len :: l

        real(k) data(l)
    end type

    type(a(4, 10)) a1

    a1%k = 8        !<-- illegal
    a1%l = 100      !<-- illegal
end
