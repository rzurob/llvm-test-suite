! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/22/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: An extended type includes all of the
!                               type parameters of its parent type.
!                               Case: use the type parameters of base type in
!                               the extended type of the 3rd generation.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamExtends003
use m1
    type (gen3(double, nameLen+5)) :: g1
    type (gen3(single, nameLen)), target :: g2(10)

    !! verify the derived type parameter values and component values
    if ((g1%k1 /= double) .or. (g1%l1 /= nameLen+5)) error stop 1_4

    b1_m => g2(3)

    if ((g2%k1 /= single) .or. (g2%l1 /= nameLen)) error stop 2_4
    if ((b1_m%k1 /= single) .or. (b1_m%l1 /= nameLen)) error stop 3_4

    if ((b1_m%id /= -1) .or. (b1_m%name /= 'namele')) error stop 4_4

    if ((g1%id /= -1) .or. (g1%name /= 'nameless')) error stop 5_4

    if (any(g1%data /= (/(1.0_8, i=1,nameLen+5)/))) error stop 6_4 !<--on purpose

    if ((any(g2%id /= -1)) .or. (any(g2%name /= 'namele'))) error stop 7_4

    if (any(g2(3)%data /= (/(1.0_8, i=1,nameLen)/))) error stop 8_4 !<--on purpose
end