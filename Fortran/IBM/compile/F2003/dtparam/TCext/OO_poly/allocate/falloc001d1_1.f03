! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc001d1_1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (C624: if a type-spec appears, it
!                               shall specify a type with which each
!                               allocate-object is type-compatible)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program falloca001d1_1
use m
    type (base(4)), pointer :: b1

    class (child(4,1,20)), allocatable :: c1
    class (*), pointer :: x(:)

    allocate (child(4,1,20) :: b1)              !<-- illegal
    allocate (base(4) :: x(100), c1)       !<-- illegal
end