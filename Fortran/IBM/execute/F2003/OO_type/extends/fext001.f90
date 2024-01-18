! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extended (component inherited,
!                                public components)
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
        integer :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type
end module

program fext001
    use m
    type (child) :: c1

    c1%base%id = 20
    c1%name = 'This is a test'

    if ( c1%base%id /= 20) error stop 1_4
    if ( c1%name /= 'This is a test') error stop 2_4
end
