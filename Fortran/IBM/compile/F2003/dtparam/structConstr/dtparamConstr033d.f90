!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of the type name before the derived
!                               type is defined.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type (base(8, 10)), parameter :: b_const = base(8, 10)(1.0)

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type
end module

program dtparamConstr033d
end
