! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc001d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (abstract type cannot be the dynamic
!                               type of the allocate-object that is allocated
!                               successfully)
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

module m
    type, abstract :: base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    class (base(4)), allocatable :: data1 (:)
end module

program falloc001d
use m

    allocate (data1(10))    !<-- this is illegal
end
