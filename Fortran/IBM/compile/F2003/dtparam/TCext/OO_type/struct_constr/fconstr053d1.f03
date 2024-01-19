! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr053d1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C488)
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
    type base(k1,k2,n1)    ! (4,1,20)
        integer, kind             :: k1,k2
        integer, len              :: n1
        integer(k1), private      :: id = 1
        character(kind=k2,len=n1) :: name
    end type
end module

program fconstr053d1
use m
    type (base(4,1,20)) :: b1 = base(4,1,20) (name = "b1")

    b1 = base(4,1,20) (id = 10, name = 'b1')   !<-- id is inaccessible

    b1 = base(4,1,20) (10, name = 'b1')        !<-- a different error message
end
