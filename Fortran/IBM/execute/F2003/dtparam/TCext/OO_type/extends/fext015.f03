! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extended (extended type has no
!*                               more components)
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
    type base(n)
        integer, len :: n
        character(n) :: name
    end type

    type, extends(base) :: child
    end type

    type(child(30)) :: c1_m
end module

program fext015
    use m

    type, extends(base) :: secondChild
    end type

    type (child(30)) :: c1
    type (base(30)) :: b1
    type (secondChild(30)) :: s1

    c1%name = 'This is a test'
    b1%name = 'this is base type'
    s1%name = 'an extended type in main'
    c1_m%name = 'an extended type in module'

    if ( c1%name /= 'This is a test') error stop 1_4

    if (b1%name /= 'this is base type') error stop 2_4

    if (s1%name /= 'an extended type in main') error stop 3_4

    if (c1_m%name /= 'an extended type in module') error stop 4_4
end
