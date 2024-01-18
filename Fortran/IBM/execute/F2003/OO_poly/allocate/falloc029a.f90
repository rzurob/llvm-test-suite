! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (length-type-parameter in type-spec in
!                               allocate statement)
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

program falloc029a
    character(10), pointer :: c1

    nullify (c1)

    call xyz(c1, 10)

    if (.not. associated (c1)) error stop 1_4

    if (c1 /= 'xlftest101') error stop 2_4

    contains

    subroutine xyz (c, l)
        character(l), pointer :: c
        integer l

        if (.not. associated (c)) allocate (character(l) :: c)

        c = 'xlftest101'
    end subroutine
end
