!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : assign LOGICAL noncoarray variables to coarray variables (scalar and array) in main program and vice-versa
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  Assign simple values to logical coarray scalars and arrays of different kinds
!*  in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csSimpleLogical

    implicit none

    logical, parameter :: F = .false., T = .true.
    logical :: T1

    logical(1), save :: l1[*] = F, la1(10)[*] = F
    logical(2), save :: l2[*] = F, la2(10)[*] = F
    logical(4), save :: l4[*] = F, la4(10)[*] = F
    logical(8), save :: l8[*] = F, la8(10)[*] = F

    ! Try to avoid optimisations (T1 is quasi-constant: since we don't run tests
    ! which provide command-line arguments, T1 will always be assigned "TRUE",
    ! but the optimiser can't know that).
    T1 = (command_argument_count() < 10)

    ! set scalars to TRUE and arrays to a mix of T/F
    l1 = T1
    l2 = T1
    l4 = T1
    l8 = T1

    la1               = [T1,T1,T1,F,T1,F,F,T1,F,F] ! sort-of Fibonacci
    la2(1:9:2)        = T1 ! odd elements
    la4([2,4,6,8,10]) = T1 ! even elements
    la8(9:1:-2)       = T1 ! odd elements again

    if (.not. l1 .or. .not. l2 .or. .not. l4 .or. .not. l8) error stop 2
    if (any(la1 .neqv. [T,T,T,F,T,F,F,T,F,F])) error stop 3
    if (any(la2 .neqv. [T,F,T,F,T,F,T,F,T,F])) error stop 4
    if (any(la4 .neqv. [F,T,F,T,F,T,F,T,F,T])) error stop 5
    if (any(la8 .neqv. [T,F,T,F,T,F,T,F,T,F])) error stop 6

    ! set scalars to FALSE and arrays to a different mix of T/F
    l1 = F
    l2 = F
    l4 = F
    l8 = F

    la1               = [F,F,F,T1,F,T1,T1,F,T1,T1] ! sort-of Fibonacci
    la2 = T1
    la2(1:9:2)        = F ! odd elements
    la4 = T1
    la4([2,4,6,8,10]) = F ! even elements
    la8 = T1
    la8(9:1:-2)       = F ! odd elements again

    if (l1 .or. l2 .or. l4 .or. l8) error stop 7
    if (any(la1 .neqv. [F,F,F,T,F,T,T,F,T,T])) error stop 8
    if (any(la2 .neqv. [F,T1,F,T1,F,T1,F,T1,F,T1])) error stop 9
    if (any(la4 .neqv. [T1,F,T1,F,T1,F,T1,F,T1,F])) error stop 10
    if (any(la8 .neqv. [F,T1,F,T1,F,T1,F,T1,F,T1])) error stop 11

end program csSimpleLogical
