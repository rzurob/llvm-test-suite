! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : extends (test of accessibility of components;
!*                               inherited components retain their
!*                               accessibilities)
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
    type base(k1)
        integer, kind :: k1
        integer(k1) :: id
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        private     !! this private does NOT affect parent component
        character(n) :: name = ''

        contains

        procedure :: assgnName => assgnName2Child
    end type

    type (child(4,20)), save :: c1_m

    private assgnName2Child

    contains

    subroutine assgnName2Child (c, s)
        class (child(4,*)), intent(inout) :: c
        character(*), intent(in) :: s

        c%name = s
    end subroutine

    logical function isChildValCorrect (c, i, s)
        type (child(4,20)), intent(in) :: c
        integer*4, intent(in) :: i
        character(*), intent(in) :: s

        isChildValCorrect = ((c%id == i) .and. (c%name == s))
    end function
end module

program fext042
use m
    type (child(4,20)) :: c1

    c1 = child (4,20)(1)

    call c1%assgnName ('c1')

    if (.not. isChildValCorrect (c1, 1, 'c1')) error stop 1_4

    c1%base = base (4)(2)

    if (.not. isChildValCorrect (c1, 2, 'c1')) error stop 2_4

    c1%id = 3

    if (.not. isChildValCorrect (c1, 3, 'c1')) error stop 3_4
end
