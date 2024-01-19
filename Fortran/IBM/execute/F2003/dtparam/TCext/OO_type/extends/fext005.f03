! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extended (component inherited,
!                                parent introduced via use, private parent components)
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
    type base(k)
        integer, kind :: k
        integer(k), private :: id = 10
    end type

    contains
        logical function isBaseCorrect (b, intVal)
            type (base(4)), intent (in) :: b
            integer(4), intent (in) :: intVal

            isBaseCorrect = (b%id .eq. intVal)
        end function
end module

module m1
use m
    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (base(4)), save :: b1_m
    type (child(4,20)), save :: c1_m

    contains
        logical function isChildCorrect (c, intVal, charVal)
            type(child(4,*)), intent (in) :: c
            integer(4), intent (in) :: intVal
            character(*), intent (in) :: charVal

            isChildCorrect = (isBaseCorrect (c%base, intVal) .and. (c%name .eq. charVal))
        end function
end module

program fext005
    use m1
    type (child(4,20)) :: c1

    c1%name = 'This is a test'
    c1_m%name = 'c1_m'

    if (.not. isChildCorrect(c1, 10_4, 'This is a test')) error stop 1_4

    if (.not. isBaseCorrect (b1_m, 10_4)) error stop 2_4

    if (.not. isChildCorrect(c1_m, 10_4, 'c1_m')) error stop 3_4
end
