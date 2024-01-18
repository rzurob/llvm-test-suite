!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_omp_f002.f
!*
!*  DATE                       : August 12, 2015
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT
!*  SECONDARY FUNCTIONS TESTED : OMP PARALLEL
!*
!*  REQUIRED COMPILER OPTIONS  : -qsmp
!*
!*  KEYWORD(S)                 : DO CONCURRENT OMP PARALLEL
!*
!*  DESCRIPTION
!*
!*  Test DO CONCURRENT loops inside OMP PARALLEL regions.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  PROGRAM main

  USE OMP_LIB
  IMPLICIT NONE

  INTEGER, PARAMETER :: C=1025, Threads=9
  INTEGER :: I, J, K, L, N=C
  INTEGER :: Work(C)=-3
  INTEGER :: TaskNum(C)=-3

  Work = -1
  TaskNum = -1

!$OMP PARALLEL num_threads(Threads) PRIVATE(TaskNum, K)

  DO CONCURRENT (J = 1:N)
    DO CONCURRENT (I = 1:1)
      K = J*I
      !$OMP TASK SHARED(TaskNum)
      TaskNum = [(K, K=1, C)]
      !$OMP END TASK
      IF ( K .NE. J*I ) ERROR STOP 11
      !$OMP TASKWAIT
      IF ( ANY( TaskNum .NE. [(K, K=1, C)] ) ) ERROR STOP 12
    END DO
  END DO

!$OMP END PARALLEL


!$OMP PARALLEL num_threads(Threads) PRIVATE(TaskNum, K)

  DO CONCURRENT (J = 1:N, I = 1:1)
    K = J*I
    !$OMP TASK SHARED(TaskNum)
      DO CONCURRENT (K=1:C)
        TaskNum(K) = K
      END DO
    !$OMP END TASK
    IF ( K .NE. J*I ) ERROR STOP 21
    !$OMP TASKWAIT
    IF ( ANY( TaskNum .NE. [(K, K=1, C)] ) ) ERROR STOP 22
  END DO

!$OMP END PARALLEL

  END
