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
!*                               can be resued)
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
    type, private :: base(ki,kv)
        integer, kind :: ki,kv
        integer(ki) :: id
        real(kv) :: value
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type
end module


program fext029
    use m

    type, extends(child) :: thirdGeneration(kl)
        integer, kind :: kl
        logical(kl) :: base
    end type


    type(thirdGeneration(4,4,20,1)) :: t1
    type (child(4,4,20)) :: c1

    t1%id = 10
    t1%value = 10.0
    t1%name = 'test data t1'
    t1%base = .true.

    c1%name = 'c1'
    c1%id = 20
    c1%value = 100.0

    if (t1%child%id /= 10) error stop 1_4
    if (t1%child%value /= 10.0) error stop 2_4
    if (t1%child%name /= 'test data t1') error stop 3_4
    if (.not. t1%base) error stop 4_4

    if (c1%id /= 20) error stop 5_4
    if (c1%value /= 100.0) error stop 6_4
    if (c1%name /= 'c1') error stop 7_4

    ! check the short hand name for t1
    if ((t1%id /= 10) .or. (t1%value /= 10.0)) error stop 8_4

end
