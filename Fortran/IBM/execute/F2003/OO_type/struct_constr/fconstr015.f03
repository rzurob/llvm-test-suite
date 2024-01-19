! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 14, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (generics takes higher
!*                               precedence)
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
    type base
        integer*4 ::id = 10
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'
    end type

end module

program fconstr015
use m

    interface child
        type (child) function c_c()
        use m
        end function
    end interface

    type (child) :: c1, c2, c3

    c1 = child()  ! this is call interface
    c3 = child(100, 'data c3')  ! this is structure constructor

    if (c1%id /= 1) error stop 1_4
    if (c1%name /= 'test') error stop 2_4

    if (c2%id /= 10) error stop 3_4
    if (c2%name /= 'default') error stop 4_4

    if (c3%id /= 100) error stop 5_4
    if (c3%name /= 'data c3') error stop 6_4
end

type (child) function c_c()
use m

    c_c = child(1, 'test')
end function

