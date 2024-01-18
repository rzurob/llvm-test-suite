! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extended (component inherited,
!*                                parent introduced via use, public components)
!*                               parent components are with pointer and
!*                               allocatable attributes
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
    type base(ki,kv)
        integer, kind :: ki,kv
        integer(ki), pointer :: id => null()
        real(kv), allocatable :: value
    end type
end module

module m1
use m
    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type
end module

program fext004a
    use m1
    type (child(4,4,20)) :: c1
    integer(4), target :: intVal = 20

    if (associated(c1%base%id)) error stop 1_4
    if (allocated (c1%base%value)) error stop 2_4

    c1%base%id => intVal
    allocate (c1%base%value)

    if (.not. associated(c1%id)) error stop 3_4
    if (.not. allocated (c1%value)) error stop 4_4

    c1%value = -1.0
    c1%name = 'This is a test'

    if (c1%base%id /= 20) error stop 5_4
    if (c1%base%value /= -1.0) error stop 6_4
    if (c1%name /= 'This is a test') error stop 7_4
    if (c1%id /= 20) error stop 8_4       ! short hand name
    if (c1%value /= -1.0) error stop 9_4  ! short hand name

end
