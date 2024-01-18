!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csLogicalArg
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass LOGICAL scalar coarray actual argument to procedure which examines it and modifies it
!*  ADAPTED FROM               : csSimpleLogical
!*
!*  DESCRIPTION
!*
!*  Invoke procedures with coarray actual arguments - they examine and modify the arguments.
!*  Arrays are already handled in many other features, so we skip these.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csLogicalArg

    implicit none

    logical, parameter :: F = .false., T = .true.
    logical :: T1, F1

    logical(1), save :: l1[*] = F
    logical(2), save :: l2[*] = F
    logical(4), save :: l4[*] = F
    logical(8), save :: l8[*] = F

    integer :: i

    ! Try to avoid optimisations (T1 is quasi-constant: since we don't run tests
    ! which provide command-line arguments, T1 will always be assigned "TRUE",
    ! but the optimiser can't know that).
    T1 = (command_argument_count() < 10)
    F1 = .not. T1

    call twiddle1(l1, F1, T1, 1)
    call twiddle1(l1, T1, F1, 2)
    call twaddle1(l1, F1, T1, 3)
    call twaddle1(l1, T1, F1, 4)
    if (fiddle1(l1,T1)) call fail(5)
    if (.not.faddle1(l1,T1)) call fail(6)

    call twiddle2(l2, F1, T1, 11)
    call twiddle2(l2, T1, F1, 12)
    call twaddle2(l2, F1, T1, 13)
    call twaddle2(l2, T1, F1, 14)
    if (fiddle2(l2,T1)) call fail(15)
    if (.not.faddle2(l2,T1)) call fail(16)

    call twiddle4(l4, F1, T1, 21)
    call twiddle4(l4, T1, F1, 22)
    call twaddle4(l4, F1, T1, 23)
    call twaddle4(l4, T1, F1, 24)
    if (fiddle4(l4,T1)) call fail(25)
    if (.not.faddle4(l4,T1)) call fail(26)

    call twiddle8(l8, F1, T1, 31)
    call twiddle8(l8, T1, F1, 32)
    call twaddle8(l8, F1, T1, 33)
    call twaddle8(l8, T1, F1, 34)
    if (fiddle8(l8,T1)) call fail(35)
    if (.not.faddle8(l8,T1)) call fail(36)

  contains

    subroutine twiddle1(a1, exp1, new1, nr)
      logical(1) :: a1
      logical :: exp1, new1
      integer :: nr
      if (a1 .neqv. exp1) call fail(nr)
      a1 = new1
    end subroutine twiddle1

    subroutine twaddle1(a1, exp1, new1, nr)
      logical(1) :: a1[*]
      logical :: exp1, new1
      integer :: nr
      if (a1 .neqv. exp1) call fail(nr)
      a1 = new1
    end subroutine twaddle1

    logical function fiddle1(a1, new1)
      logical(1) :: a1
      logical :: new1
      fiddle1 = a1
      a1 = new1
    end function fiddle1

    logical function faddle1(a1, new1)
      logical(1) :: a1[*]
      logical :: new1
      faddle1 = a1
      a1 = new1
    end function faddle1


    subroutine twiddle2(a2, exp2, new2, nr)
      logical(2) :: a2
      logical :: exp2, new2
      integer :: nr
      if (a2 .neqv. exp2) call fail(nr)
      a2 = new2
    end subroutine twiddle2

    subroutine twaddle2(a2, exp2, new2, nr)
      logical(2) :: a2[*]
      logical :: exp2, new2
      integer :: nr
      if (a2 .neqv. exp2) call fail(nr)
      a2 = new2
    end subroutine twaddle2

    logical function fiddle2(a2, new2)
      logical(2) :: a2
      logical :: new2
      fiddle2 = a2
      a2 = new2
    end function fiddle2

    logical function faddle2(a2, new2)
      logical(2) :: a2[*]
      logical :: new2
      faddle2 = a2
      a2 = new2
    end function faddle2


    subroutine twiddle4(a4, exp4, new4, nr)
      logical(4) :: a4
      logical :: exp4, new4
      integer :: nr
      if (a4 .neqv. exp4) call fail(nr)
      a4 = new4
    end subroutine twiddle4

    subroutine twaddle4(a4, exp4, new4, nr)
      logical(4) :: a4[*]
      logical :: exp4, new4
      integer :: nr
      if (a4 .neqv. exp4) call fail(nr)
      a4 = new4
    end subroutine twaddle4

    logical function fiddle4(a4, new4)
      logical(4) :: a4
      logical :: new4
      fiddle4 = a4
      a4 = new4
    end function fiddle4

    logical function faddle4(a4, new4)
      logical(4) :: a4[*]
      logical :: new4
      faddle4 = a4
      a4 = new4
    end function faddle4


    subroutine twiddle8(a8, exp8, new8, nr)
      logical(8) :: a8
      logical :: exp8, new8
      integer :: nr
      if (a8 .neqv. exp8) call fail(nr)
      a8 = new8
    end subroutine twiddle8

    subroutine twaddle8(a8, exp8, new8, nr)
      logical(8) :: a8[*]
      logical :: exp8, new8
      integer :: nr
      if (a8 .neqv. exp8) call fail(nr)
      a8 = new8
    end subroutine twaddle8

    logical function fiddle8(a8, new8)
      logical(8) :: a8
      logical :: new8
      fiddle8 = a8
      a8 = new8
    end function fiddle8

    logical function faddle8(a8, new8)
      logical(8) :: a8[*]
      logical :: new8
      faddle8 = a8
      a8 = new8
    end function faddle8

    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end program csLogicalArg
