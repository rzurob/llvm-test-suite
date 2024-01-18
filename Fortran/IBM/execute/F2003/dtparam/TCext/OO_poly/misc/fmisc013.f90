! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/misc/fmisc013.f
! opt variations: -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 293424, last part)
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

program fmisc013
    type :: A(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: i
    end type

    type(A(20,4)), pointer :: a1
    integer, pointer :: i1

    allocate(a1)
    allocate(a1%i)

    a1%i = 100

    i1 => a1%i   !<-- a1%i has target attribute implicitly

    if (i1 /= 100) error stop 1_4
end
