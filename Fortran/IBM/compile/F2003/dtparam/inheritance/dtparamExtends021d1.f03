! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/12/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: parameterized sequence type being extended
!                               to another sequence type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamExtends021d1
    type seq1 (k, l)
        integer, kind :: k = 4
        integer, len  :: l = 10

        sequence

        integer(k) id
        character(l) name
        real(k) data(l)
    end type

    type, extends(seq1) :: seq2 (k2, l2)    !<-- illegal
        integer, kind :: k2
        integer, len  :: l2

        sequence

        complex(k2) cx(l2)
    end type
end
