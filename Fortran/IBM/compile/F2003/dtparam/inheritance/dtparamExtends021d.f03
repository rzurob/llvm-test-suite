! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/12/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: a derived type tries to self-extend
!                               itself.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamExtends021d
    type A (k1, l1)
        integer, kind :: k1
        integer, len  :: l1 = 10

        integer(k1) data(l1)
    end type

    type, extends(A) :: A (k2, l2)  !<-- self-extension
        integer, kind :: k2
        integer, len  :: l2
    end type
end