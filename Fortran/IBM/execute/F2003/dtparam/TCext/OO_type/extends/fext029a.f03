! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 09, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (private parent
!*                               type results in private parent component; name
!*                               can be resued; original base type with type
!*                               bound procedure only)
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
    type, private :: base(k)
        integer, kind :: k
        contains

        procedure, nopass :: value => baseValue
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    contains

    subroutine baseValue()
        print *, 1
    end subroutine
end module


program fext029a
    use m

    type, extends(child) :: thirdGeneration(kl)
        integer, kind :: kl
        logical(kl) :: base
    end type


    type(thirdGeneration(4,20,1)) :: t1
    type (child(4,20)) :: c1

    c1%name = 'c1'

    t1%name = 't1'
    t1%base = .true.

    if (c1%name /= 'c1') error stop 1_4

    if (t1%name /= 't1') error stop 2_4
    if (.not. t1%base) error stop 3_4

    call c1%value
    call t1%value
end