!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 20, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   : Submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qsmp
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  based on omp31/Atomic/func/AtomicCapture06f.f
!*
!*  This testcase tests the acceptance of the constructs:
!*  - Atomic capture
!*  - OMP Parallel loop
!*  - Scalar pointer
!*  in submodules.
!*
!*  Secondary tests:
!*  - use of a module with descendant submodules in another module with
!*    descendant submodules, which is used in another scope
!*  - mutations in a variable use-associated from same module can be
!*    confirmed in other modules where it is use-associated.  It can be
!*     mutated:
!*     - directly in the submodule scope by assignment
!*     - indirectly in the submodule scope by calling a procedure on the
!*       use-associated module (the procedure may reside in a submodule)
!*     - indirectly through OMP parallel as a shared variable (also
!*       verify it is not modified with OMP parallel private)
!*
!*  Note: the subroutines are numbered in order of execution.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE data
IMPLICIT NONE

  IMPLICIT NONE
  INTEGER, PARAMETER :: N=1100
  INTEGER, POINTER :: p
  INTEGER, TARGET :: t, t2
  INTEGER :: i, q

END MODULE

!---------------------------------------------------------------------
! Define the first module and descendant submodules, mod1, which will!
! be used by mod2.                                                   !
!                          mod1                                      !
!                          /  \                                      !
!                         /    \                                     !
!                        /      \                                    !
!                  smod1_1      smod1_2                              !
!                                /  \                                !
!                               /    \                               !
!                              /      \                              !
!                        smod1_3      smod1_4                        !
!                                      /  \                          !
!                                     /    \                         !
!                                    /      \                        !
!                              smod1_5      smod1_6                  !
!---------------------------------------------------------------------
MODULE mod1
  USE data
  IMPLICIT NONE

  INTERFACE
    module subroutine initializeEnv()
      USE OMP_LIB
    end subroutine

    module subroutine sub02()
    end subroutine

    module subroutine sub04()
    end subroutine

    module subroutine sub06()
    end subroutine

    module subroutine sub08()
    end subroutine
  END INTERFACE
END MODULE

SUBMODULE (mod1) smod1_1
CONTAINS
  module procedure initializeEnv
    USE OMP_LIB
    CALL omp_set_dynamic(.true.)
    IF (.NOT. omp_get_dynamic()) error stop 1

    CALL omp_set_num_threads(omp_get_num_procs())
  end
END SUBMODULE

SUBMODULE (mod1) smod1_2
CONTAINS
  module procedure sub02
    if ((q  .lt. 0) .and. (q .gt. p-1)) error stop 2
    if (p   .ne. 1000) error stop 3
  end
END SUBMODULE smod1_2

SUBMODULE (mod1:smod1_2) smod1_3
CONTAINS
  module subroutine sub04()
    if ((q  .lt. 0) .and. (q .gt. p)) error stop 4
    if (p   .ne. 1000) error stop 5
  end subroutine sub04
END SUBMODULE

SUBMODULE (mod1:smod1_2) smod1_4
CONTAINS
  module procedure sub06
    if (p   .ne. -100) error stop 8
    if (t   .ne. -100) error stop 9
    if ((q  .lt. 0) .and. (q .gt. t2-1)) error stop 10
    if (t2  .ne. 1099) error stop 11
    if (.not. associated(p,t))  error stop 12
  end procedure sub06
END SUBMODULE

SUBMODULE (mod1:smod1_4) smod1_5
CONTAINS
  module subroutine sub08()
    if (p   .ne. -100) error stop 15
    if (t   .ne. -100) error stop 16
    if ((q  .lt. 0) .and. (q .gt. t2)) error stop 17
    if (t2  .ne. 1099) error stop 18
    if (.not. associated(p,t))  error stop 19
  end subroutine sub08
END SUBMODULE

!---------------------------------------------------------------------
! Define the 2nd  module and descendant submodules, mod1, which will !
! be used by main program.                                           !
!                               mod2                                 !
!                               /  \                                 !
!                              /    \                                !
!                             /      \                               !
!                         smod2_1  smod2_2                           !
!                           /  \                                     !
!                          /    \                                    !
!                         /      \                                   !
!                     smod2_3   smod2_4                              !
!                       /  \                                         !
!                      /    \                                        !
!                     /      \                                       !
!                 smod2_5   smod2_6                                  !
!---------------------------------------------------------------------
MODULE mod2
  USE OMP_LIB
  USE mod1
  IMPLICIT NONE

  INTERFACE
    module subroutine sub01()
    end subroutine

    module subroutine sub03()
    end subroutine

    module subroutine sub05()
    end subroutine

    module subroutine sub07()
    end subroutine

    module subroutine sub09()
    end subroutine
  END INTERFACE
END MODULE

SUBMODULE (mod2) smod2_1
  CONTAINS
    module procedure sub01
      call initializeEnv()

      q = 0; t = -100
      p => t
      !$OMP PARALLEL DO SHARED(p)
          DO i = 1, N
                !$OMP ATOMIC CAPTURE
                       q = p
                       p = p + 1
                !$OMP END ATOMIC
          END DO
      !$OMP END PARALLEL DO

      call sub02()
      call sub03()
    end
END SUBMODULE

SUBMODULE (mod2) smod2_2
  CONTAINS
    module subroutine sub03()
      q = 0; t = -100
      !$OMP PARALLEL DO SHARED(p)
          DO i = 1, N
                !$OMP ATOMIC CAPTURE
                       p = p + 1
                       q = p
                !$OMP END ATOMIC
          END DO
      !$OMP END PARALLEL DO

      call sub04()
      call sub05()
    end subroutine sub03
END SUBMODULE

SUBMODULE (mod2:smod2_1) smod2_3
  CONTAINS
    module procedure sub05

      q = 0; t = -100
      t2 = -1
      p => t
      if (.not. associated(p,t))  error stop 6
      !$OMP PARALLEL PRIVATE(p)
            p => t2
            !$OMP CRITICAL
                if (.not. associated(p,t2))  error stop 7
            !$OMP END CRITICAL

            !$OMP DO
                DO i = 1, N
                      !$OMP ATOMIC CAPTURE
                             q = p
                             p = p + 1
                      !$OMP END ATOMIC
                END DO
            !$OMP END DO
      !$OMP END PARALLEL

      call sub06()
      call sub07()
    end
END SUBMODULE

SUBMODULE (mod2:smod2_1) smod2_4
  INTERFACE
    module subroutine helper()
    end subroutine
  END INTERFACE

  CONTAINS
    module procedure sub07
      q = 0; t = -100
      t2 = -1
      p => t
      if (.not. associated(p,t))  error stop 13
      !$OMP PARALLEL PRIVATE(p)
            p => t2
            !$OMP CRITICAL
                if (.not. associated(p,t2))  error stop 14
            !$OMP END CRITICAL

            !$OMP DO
                DO i = 1, N
                      !$OMP ATOMIC CAPTURE
                             p = p + 1
                             q = p
                      !$OMP END ATOMIC
                END DO
            !$OMP END DO
      !$OMP END PARALLEL

      call helper()
      call sub08()
      call sub09()
    end
END SUBMODULE

! local scope
SUBMODULE (mod2:smod2_4) smod2_7
  INTEGER, PARAMETER :: N=1000
  INTEGER, POINTER :: p
  INTEGER, TARGET :: t, t2
  INTEGER :: i, q
  CONTAINS
    module procedure helper
      q = 0; t = -50
      t2 = -1
      p => t
      if (.not. associated(p,t))  error stop 113
      !$OMP PARALLEL PRIVATE(p)
            p => t2
            !$OMP CRITICAL
                if (.not. associated(p,t2))  error stop 114
            !$OMP END CRITICAL

            !$OMP DO
                DO i = 1, N
                      !$OMP ATOMIC CAPTURE
                             p = p + 1
                             q = p
                      !$OMP END ATOMIC
                END DO
            !$OMP END DO
      !$OMP END PARALLEL

      if (p   .ne. -50) error stop 115
      if (t   .ne. -50) error stop 116
      if ((q  .lt. 0) .and. (q .gt. t2)) error stop 117
      if (t2  .ne. 999) error stop 118
      if (.not. associated(p,t))  error stop 119

    end
END SUBMODULE

SUBMODULE (mod2:smod2_3) smod2_5
  CONTAINS

    module procedure sub09
      print*, "+++ test successful"
    end

END SUBMODULE

PROGRAM module_subprogram07f
USE mod2
IMPLICIT NONE
call sub01()
END PROGRAM module_subprogram07f