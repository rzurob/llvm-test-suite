!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_omp_f006.f
!*
!*  PROGRAMMER                 : Nicole Negherbon
!*  DATE                       : August 24, 2015
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT in OMP directives
!*  SECONDARY FUNCTIONS TESTED : OMP 3.0 loop collapse clause
!*
!*  REFERENCE                  : omp30/loopCollapse/loopCollapse/loop_collapse_task_27.f
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : -qsmp
!*
!*  KEYWORD(S)                 : DO CONCURRENT, OMP, TASK, CRITICAL, TASKWAIT
!*
!*  DESCRIPTION
!*   
!*  Rules for variables with implicitly determined data-sharing attributes: 
!*  In a parallel or task construct, the data-sharing attributes of these variables are
!*  determined by the default clause, if present.
!* 
!*  (346655)
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  
  PROGRAM do_concurrent_omp_f006
  USE OMP_LIB

  CALL S()

  CONTAINS 

  SUBROUTINE S()
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: C=125, Threads=11
  INTEGER :: I, J, K, L, N=C
  INTEGER :: Work(C)=-3
  INTEGER :: TaskNum(C)=-3

  Work = -1
  TaskNum = 0 
 
!$OMP PARALLEL num_threads(Threads)

  DO CONCURRENT (J=1:N)
    DO CONCURRENT (I=1:1)
    !$OMP TASK PRIVATE(TaskNum)
      TaskNum = 0
      DO CONCURRENT (K=1:N) 
        !$OMP TASK DEFAULT(SHARED)
          !$OMP CRITICAL 
            TaskNum = TaskNum + 1
          !$OMP END CRITICAL 
        !$OMP END TASK
      END DO
      !$OMP TASKWAIT
      IF ( ANY( TaskNum .NE. N ) ) ERROR STOP 11
    !$OMP END TASK
    END DO
  END DO

!$OMP END PARALLEL 


!$OMP PARALLEL num_threads(Threads)

  DO CONCURRENT (J=1:N)
    DO CONCURRENT (I=1:1)
      !$OMP TASK PRIVATE(TaskNum)
        TaskNum = J
        DO CONCURRENT (K=1:N) 
          !$OMP TASK DEFAULT(FIRSTPRIVATE)
          IF ( ANY( TaskNum .NE. J ) ) ERROR STOP 12
          !$OMP END TASK
        END DO
      !$OMP END TASK
    END DO
  END DO

!$OMP END PARALLEL 


!$OMP PARALLEL num_threads(Threads)

  DO CONCURRENT (J=1:N)
    DO CONCURRENT (I=1:1)
      !$OMP TASK PRIVATE(TaskNum)
        TaskNum = J
        DO CONCURRENT (K=1:N) 
          !$OMP TASK DEFAULT(PRIVATE)
            TaskNum = -1
          !$OMP END TASK
          IF ( ANY( TaskNum .NE. J ) ) ERROR STOP 13
        END DO
      !$OMP END TASK
    END DO
  END DO

!$OMP END PARALLEL 

  END SUBROUTINE

  END
