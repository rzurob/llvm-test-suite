! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr051d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (at most one component-spec shall
!*                               be provided for a component)
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

program fconstr051d
    type base(k1,k2,n1)    ! (4,1,20)
        integer, kind             :: k1,k2
        integer, len              :: n1
        integer(k1)               :: id
        character(kind=k2,len=n1) :: name
    end type

    type (base(4,1,20)) :: b1 = base(4,1,20) (1, name='b1', id = 2)
end