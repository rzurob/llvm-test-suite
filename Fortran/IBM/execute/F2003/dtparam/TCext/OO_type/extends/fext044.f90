! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : extends (rename with use-only in the separate
!*                               module; also try to confuse compiler about the
!*                               name of the base type)
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
    type base0(k1)
        integer, kind :: k1
        integer(k1) :: id = 0
    end type

    type, extends(base0) :: child(n)
        integer, len :: n
        character(n) :: name = 'no-name'
    end type
end module

module m1
use m, only : base => child
    type, extends(base) :: child(k2)
        integer, kind :: k2
        logical(k2) :: flag = .false.
    end type
end module

program fext044
use m1
    type (base(4,20)) :: b1, b2
    type (child(4,20,1)) :: c1, c2

    b1 = base (4,20)(id = 1, name = 'b1')

    b2 = base (4,20)(2, 'b2')

    c1 = child (4,20,1)(base0 = b1%base0, name = 'c1', flag = .true.)

    c2 = child (4,20,1)(base = b2, flag = .true.)

    !! validate the data
    if ((b1%id /= 1) .or. (b1%name /= 'b1')) error stop 1_4

    if ((b2%base0%id /= 2) .or. (b2%name /= 'b2')) error stop 2_4

    if ((c1%id /= 1) .or. (c1%name /= 'c1') .or. (.not. c1%flag)) &
        error stop 3_4

    if ((c2%base%base0%id /= 2) .or. (c2%base%name /= 'b2') .or. &
        (.not. c2%flag))  error stop 4_4
end
