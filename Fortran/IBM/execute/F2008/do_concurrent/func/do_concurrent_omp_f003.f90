!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 12, 2015
!*
!*  PRIMARY FUNCTIONS TESTED   : OMP 3.0 loop collapse clause
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qsmp
!*
!*  REFERENCE                  : omp30/loopCollapse/loopCollapse/loop_collapse_threadprivate_3.scenario
!*
!*  DESCRIPTION                :
!*
!*  Tests DO CONCURRENT with THREADPRIVATE and PARALLEL
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  PROGRAM main
  USE OMP_LIB
  IMPLICIT NONE

  INTEGER, PARAMETER :: C=32
  INTEGER :: N
  INTEGER :: I, J, K, L, M, Mark=0, Junk(C)=0
  INTEGER, TARGET      :: T
  INTEGER, POINTER     :: P1
  INTEGER, POINTER     :: P2
  COMMON  /MyCommon/P1, P2
!$OMP THREADPRIVATE (/MyCommon/)

  N = C

! The values of data in the threadprivate objects of non-initial threads are guaranteed to
! persist between two consecutive active parallel regions only if all the following
! conditions hold:
! . Neither parallel region is nested inside another explicit parallel region.
! . The number of threads used to execute both parallel regions is the same.
! . The value of the dyn-var internal control variable in the enclosing task region is false
! at entry to both parallel regions.
! If these conditions all hold, and if a threadprivate object is referenced in both regions,
! then threads with the same thread number in their respective regions will reference the
! same copy of that variable.

! If the above conditions hold, the definition, association, or allocation status of a thread.s
! copy of a threadprivate variable or a variable in a threadprivate common block, that is
! not affected by any copyin clause that appears on the second region, will be retained.

  T = -1
  P1 => T
  P2 => P1

  CALL omp_set_dynamic (.false.)

!$OMP PARALLEL copyin(P1, P2) num_threads(11)
  DO CONCURRENT (K = 1:N)
    DO CONCURRENT (J = N:1:-1)
      DO CONCURRENT (I = 1:N)
        IF ( .NOT. ASSOCIATED(P1, T)  ) ERROR STOP 11
        IF ( .NOT. ASSOCIATED(P1, P2) ) ERROR STOP 12
        P2 = -2
      END DO
    END DO
  END DO
!$OMP END PARALLEL

  CALL omp_set_dynamic (.false.)

!$OMP PARALLEL num_threads(11)
  DO CONCURRENT (K = 1:N, J = N:1:-1)
    IF ( .NOT. ASSOCIATED(P1, T)  ) ERROR STOP 21
    IF ( .NOT. ASSOCIATED(P1, P2) ) ERROR STOP 22
    IF ( T   .NE. -2  )             ERROR STOP 23
  END DO
!$OMP END PARALLEL

  END
