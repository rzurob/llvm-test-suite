! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-29
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : this is the preliminary work for
!                               five_point_stencil.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
program test_field_distribution
    use constants_mod, only: double, pi
    use distribute_fiele_mod, only : initialize_field, dist
    use verify_field_mod, only : verify_field
    implicit none
    intrinsic dcos
    logical, external :: precision_r8

    real(double), save :: field(101)[*]

    field(101) = 10.0d0
    call initialize_field(size(field)-1, field, 0.0d0, 2.0d0*pi, dcos)

    call verify_field (field, size(field)-1, dcos, 0.0d0, 2.0d0*pi, 5.0d-14)

    if (.not. precision_r8(field(101), 10.0d0)) then
        print *, 'last element failed test'
        error stop 1
    end if
end
