! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/implicit/fimplct003.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implied objects used in function
!*                               calls)
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
        integer(k1)   :: id = 0
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name = ''
    end type
end module

program fimplct003
use m
    implicit type (base(4)) (b), type (child(4,20)) (c)

    interface
        logical function isEqual (b1, b2)
        use m
            class (base(4)), intent(in) :: b1, b2
        end function
    end interface

    if (.not. (isEqual (b1, b2))) error stop 1_4

    if (.not. (isEqual (c1, c2))) error stop 2_4

    if (.not. (isEqual (c1%base, c2%base))) error stop 3_4

    if (.not. (isEqual (b1, c1))) error stop 4_4

    if (isEqual (b1, base(4)(10))) error stop 5_4

    if (isEqual (child(4,20)(10, 'child_c1'), base(4)(1))) error stop 6_4
end

logical function isEqual (b1, b2)
use m
    class (base(4)), intent(in) :: b1, b2

    isEqual = (b1%id == b2%id)
end function
