! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extended (component inherited,
!*                               private components)
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

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (child(4,20)), save :: c1_m

    contains
        logical function isChildCorrect (c, intVal, charVal)
            type(child(4,20)), intent (in) :: c
            integer, intent (in) :: intVal
            character(*), intent (in) :: charVal

            isChildCorrect = ((c%base%id .eq. intVal) .and. (c%name .eq. charVal))
        end function
end module

program fext002
    use m
    type (child(4,20)) :: c1

    c1%name = 'This is a test'
    c1_m%name = 'c1_m'

    if (.not. isChildCorrect(c1, 10, 'This is a test')) error stop 1_4

    if (.not. isChildCorrect(c1_m, 10, 'c1_m')) error stop 2_4
end
