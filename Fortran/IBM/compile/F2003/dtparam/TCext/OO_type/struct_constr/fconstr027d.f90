! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr027d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C487)
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

        contains
            procedure, nopass :: value => baseValue
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type, extends(child) :: thirdGeneration(k3)    ! (4,1,20,1)
        integer, kind :: k3
        logical(k3)   :: isSet
    end type

    contains

        real*4 function baseValue ()
            baseValue = 1.0
        end function
end module


program fconstr027d
use m

    type (thirdGeneration(4,1,20,1)), pointer :: t1 = thirdGeneration(4,1,20,1) (1, value = 2.0, &
                name = 't1', isSet = .true.)

    t1 = thirdGeneration(4,1,20,1) (isSet = .true., name='t1', id = 1, value = baseValue)
end
