! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 10, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (overriding accessiblity of a
!*                               module from private to public for a derived
!*                               type)
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
private
    type, public :: base(k)
        integer, kind :: k
        integer(k) :: id
    end type
end module


program fext032
use m

    type, extends (base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (base(4)) :: b1
    type (child(4,20)) :: c1

    b1%id = 10

    c1%id = 100
    c1%name = 'test data c1'

    if (b1%id /= 10) error stop 1_4

    if (c1%base%id /= 100) error stop 2_4
    if (c1%name /= 'test data c1') error stop 3_4
end
