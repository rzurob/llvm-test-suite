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
    type base
        integer*4 :: id
    end type

    type, extends(base), public :: child
        character(20) :: name
    end type
end module


program fext032a
use m

    type, extends (child) :: thirdGeneration
        logical*1 :: isSet
    end type

    type (child) :: c1
    type (thirdGeneration) :: t1

    c1%name = 'c1'
    c1%id = 1

    t1%id = 2
    t1%name = 't1'
    t1%isSet = (t1%id == (1+c1%id))

    ! validate all variables
    if (c1%id /= 1) error stop 1_4
    if (c1%name /= 'c1') error stop 2_4

    if (t1%id /= 2) error stop 3_4
    if (t1%name /= 't1') error stop 4_4
    if (.not. t1%isSet) error stop 5_4

    if (t1%id /= t1%child%id) error stop 6_4
end
