! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd510d.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (protected attribute
!*                               entities to be modified via type-bound)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: assgn => assgnBase
    end type

    type (base(20,4)), protected :: b1_pro

    contains

    subroutine assgnBase (b, i)
        class (base(*,4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

end module

program ftpbnd510d
use m
    call b1_pro%assgn(10) !<-- should fail

end