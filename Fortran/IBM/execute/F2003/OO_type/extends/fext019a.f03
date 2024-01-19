! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (base type is renamed
!*                               via use association. Test type matches for parent
!*                               component in the external procedures)
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
        integer*4 :: id = 1
    end type
end module

module m1
use m, oldBase => base
    type, extends(oldBase) :: child
        character(20) :: name
    end type

    type (child), save :: c1_m
    type (oldBase), save :: b1_m
end module


program fext019a
    use m1

    interface
    ! Note this function uses module m, not m1
        logical function valueMatch (b, intVal)
            use m
            type(base), intent(in) :: b
            integer*4, intent(in) :: intVal
        end function

        logical function childMatch (c, intVal, charVal)
            use m1
            type (child), intent(in) :: c
            integer*4, intent(in) :: intVal
            character(*), intent(in) :: charVal
        end function
    end interface

    type (oldBase) :: o1
    type (child) :: c1

    o1%id = 100
    c1%oldbase%id = 20
    c1%name = 'c1'

    if (.not. valueMatch (o1, 100)) error stop 1_4

    if (.not. valueMatch (c1%oldbase, 20)) error stop 2_4

    c1%id = 10
    if (.not. valueMatch (c1%oldbase, 10)) error stop 3_4

    c1_m%id = 2
    c1_m%name = 'c1_m'
    b1_m%id = 5

    if (.not. valueMatch (b1_m, 5)) error stop 4_4
    if (.not. valueMatch (c1_m%oldbase, 2)) error stop 5_4

    if (.not. childMatch (c1, 10, 'c1')) error stop 6_4
    if (.not. childMatch (c1_m, 2, 'c1_m')) error stop 7_4
end

logical function valueMatch (b, intVal)
use m
    type(base), intent(in) :: b
    integer*4, intent(in) :: intVal

    valueMatch = (b%id .eq. intVal)
end function

logical function childMatch (c, intVal, charVal)
use m1
    type(child), intent(in) :: c
    integer*4, intent(in) :: intVal
    character(*), intent(in) :: charVal

    type (oldBase) :: b_temp
    logical valueMatch

    b_temp = c%oldbase

    childMatch = (valueMatch (b_temp, intVal) .and. (c%name == charVal))
end function
