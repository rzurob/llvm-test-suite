!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext020.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (An extended type
!*                               introduced via use statement and renamed; extended
!*                               type's parent name and its parent name)
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
        integer, kind :: ki, kv
        integer(ki) :: id
        real(kv) :: value
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (base(4,4)) :: b0_m
    type (child(4,4,20)) :: c0_m
end module

module m1
    use m, newBase => child, oldBase => base

    type, extends(newBase) :: newChild(kl)
        integer, kind :: kl
        logical(kl) :: isSet = .false.
    end type

    type, extends (oldBase) :: secondChild(kls)
        integer, kind :: kls
        logical(kls) :: flag = .false.
    end type

    type (secondChild(4,4,1)), save :: c1_m
    type (newChild(4,4,20,2)), save :: nc1_m
end module

program fext020
    use m1

    type (newBase(4,4,20)) :: c1     !<-- actually a child type
    type (oldBase(4,4)) :: b1        !<-- base type
    type (newChild(4,4,20,2)) :: nc1 !<-- thirdGeneration
    type (secondChild(4,4,1)) :: s1  !<-- a second child type

    b1%id = 10
    b1%value = 1.0

    if (b1%id /= 10) error stop 1_4
    if (b1%value /= 1.0) error stop 2_4

    c1%base = b1
    c1%name = 'child data'
    if (c1%id /= 10) error stop 3_4
    if (c1%value /= 1.0) error stop 4_4
    if (c1%name /= 'child data') error stop 5_4

    ! for c1 id can be referred to as c1%id, c1%base%id
    if (c1%id /= c1%base%id) error stop 6_4
    if (c1%value /= c1%base%value) error stop 6_4

    nc1%base%id = 20
    nc1%base%value = 10.0
    nc1%newbase%name = 'newChild data'
    nc1%isSet = .true.

    if (nc1%newbase%id /= 20) error stop 7_4
    if (nc1%newbase%base%value /= 10.0) error stop 8_4
    if (nc1%newbase%name /= 'newChild data') error stop 9_4
    if (.not. nc1%isSet) error stop 10_4

    ! for nc1, its id component can be referred to as
    ! nc1%id, nc1%base%id, nc1%newbase%id, nc1%newbase%base%id
    if ( (nc1%id /= nc1%base%id) .or. (nc1%id /= nc1%newbase%id) .or. &
        &(nc1%id /= nc1%newbase%base%id) ) error stop 11_4

    if ( (nc1%value /= nc1%base%value) .or. (nc1%value /= nc1%newbase%value) .or.&
        &(nc1%value /= nc1%newbase%base%value) ) error stop 12_4

    ! for nc1, its component name can be referred to as
    ! nc1%name or nc1%newbase%name
    if ( nc1%name /= nc1%newbase%name) error stop 13_4

    b0_m%id = 100
    b0_m%value = 100.0

    c0_m%id = 200
    c0_m%name = 'c0_m'
    c0_m%value = 100.0 + 100.0

    s1%oldbase = b0_m
    s1%flag = (s1%id == b0_m%id)

    if (b0_m%id /= 100) error stop 14_4
    if (b0_m%value /= 100.0) error stop 15_4

    if (c0_m%id /= 200) error stop 16_4
    if (c0_m%value /= 200.0) error stop 17_4
    if (c0_m%name /= 'c0_m') error stop 18_4
    if (c0_m%id /= c0_m%base%id) error stop 19_4
    if (c0_m%value /= c0_m%base%value) error stop 20_4

    if (s1%id /= 100) error stop 21_4
    if (s1%value /= 100.0) error stop 22_4
    if (.not. s1%flag) error stop 23_4
    if (s1%id /= s1%oldbase%id) error stop 24_4
    if (s1%value /= s1%oldbase%value) error stop 25_4
end
