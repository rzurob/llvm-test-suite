! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005d1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (for allocatable dummy-arg
!*                               the declared type of the actual-arg shall be
!*                               the same as that of the dummy-arg)
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

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    subroutine abc (b1)
        class (base(4)), allocatable :: b1
    end subroutine

    subroutine cba (b2)
        class (base(4)), allocatable :: b2(:)
    end subroutine
end module

program fArg005d1
use m
    class (child(4,1,:)), allocatable :: c1
    class (child(4,1,:)), allocatable :: c2(:)

    call abc (c1)

    call cba (c2)
end
