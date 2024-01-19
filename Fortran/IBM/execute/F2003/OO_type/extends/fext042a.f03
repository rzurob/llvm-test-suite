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
    type, private :: base
        integer*4 :: id
    end type

    type, extends(base) :: child
        private     !! this private does NOT affect parent's component id
        character*20 :: name = ''

        contains

        procedure :: assgnName => assgnName2Child
    end type

    type (base) :: b1_m
    type (child), save :: c1_m

    private assgnName2Child

    contains

    subroutine initializeC1_m
        c1_m%id = 10
        c1_m%name = 'c1_m'
    end subroutine

    subroutine assgnName2Child (c, s)
        class (child), intent(inout) :: c
        character(*), intent(in) :: s

        c%name = s
    end subroutine

    logical function isChildValCorrect (c, i, s)
        type (child), intent(in) :: c
        integer*4, intent(in) :: i
        character(*), intent(in) :: s

        isChildValCorrect = ((c%id == i) .and. (c%name == s))
    end function
end module

program fext042a
use m
    type (child) :: c1

    c1 = child (id = 1)

    call c1%assgnName ('c1')

    if (.not. isChildValCorrect (c1, 1, 'c1')) error stop 1_4

    c1%id = 2

    if (.not. isChildValCorrect (c1, 2, 'c1')) error stop 2_4

    call initializeC1_m

    if (.not. isChildValCorrect (c1_m, 10, 'c1_m')) error stop 3_4

    b1_m%id = 20

    if (b1_m%id /= 2*c1_m%id) error stop 4_4
end
