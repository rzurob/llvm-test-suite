! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr046.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (structure constructor
!*                               used as actual argument)
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
    type base(k1,k2,k3,n1)    ! (4,4,1,20)
        integer, kind             :: k1,k2,k3
        integer, len              :: n1
        integer(k1), pointer      :: i1 => null()
        integer(k2)               :: id = 0
        character(kind=k3,len=n1) :: name = ''
    end type

    contains

    subroutine printBase (b)
        class (base(4,4,1,*)), intent(in) :: b

        if (associated (b%i1)) then
            print *, b%i1, b%id, b%name
        else
            print *, 'null', b%id, b%name
        end if
    end subroutine
end module

program fconstr046
use m
    integer*4, target :: i1 = 10

    call printBase (base(4,4,1,20) ())

    call printBase (base(4,4,1,20)(name = 'temp', id = 10, i1 = i1))

    call printBase (base(4,4,1,20)(null()))
end
