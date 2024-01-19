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
    type base(k)
        integer, kind :: k
        integer(k) :: id
    end type

    type, extends(base), public :: child(n)
        integer, len :: n
        character(n) :: name
    end type
end module


program fext032a
use m

    type, extends (child) :: thirdGeneration(kl)
        integer, kind :: kl
        logical(kl) :: isSet
    end type

    type (child(4,20)) :: c1
    type (thirdGeneration(4,20,1)) :: t1

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
