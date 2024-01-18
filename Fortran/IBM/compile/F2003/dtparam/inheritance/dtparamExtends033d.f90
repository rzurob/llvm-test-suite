!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/20/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: diagnostic.  Parameterized sequency type
!                               used in CLASS(derived-type-spec).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamExtends033d
    type base(k)
        integer, kind :: k

        sequence

        integer(k) :: id
        type (base(k)), pointer :: somewhere    !<-- legal
        class(base(k)), pointer :: next     !<-- illegal
    end type
    end
