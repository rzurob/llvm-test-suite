!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_omp_f005.f
!*
!*  PROGRAMMER                 : Nicole Negherbon
!*  DATE                       : August 12, 2015
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT
!*  SECONDARY FUNCTIONS TESTED : preserving value of private variables
!*
!*  DRIVER STANZA              : xlf2008_r 
!*  REQUIRED COMPILER OPTIONS  : -qsmp
!*
!*  KEYWORD(S)                 : DO CONCURRENT, OMP PARALLEL, PRIVATE
!*
!*  DESCRIPTION
!*
!*  Parallel construct and Lastprivate clause with DO CONCURRENT loops
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM main
      USE OMP_LIB
      IMPLICIT NONE
      INTEGER, PARAMETER :: N=100, Threads=4
      INTEGER :: I, J, K, L
      INTEGER, TARGET  :: tgt1, tgt2
      INTEGER, POINTER :: ptr => NULL()

      !$OMP PARALLEL NUM_THREADS(Threads) PRIVATE(ptr) 
        DO CONCURRENT (I = 1:N)
          ptr => tgt1
        END DO
      !$OMP END PARALLEL
      IF ( ASSOCIATED(ptr) )  ERROR STOP 10

      !$OMP PARALLEL NUM_THREADS(Threads) PRIVATE(ptr)
        DO CONCURRENT (I = 1:N, J = 1:10)
          ptr => tgt1
        END DO
      !$OMP END PARALLEL
      IF ( ASSOCIATED(ptr) )  ERROR STOP 11


      !$OMP PARALLEL NUM_THREADS(Threads) FIRSTPRIVATE(ptr) 
        DO CONCURRENT (I = 1:N)
          DO CONCURRENT (J = 1:5)
            DO CONCURRENT (K = 1:5)
              ptr => tgt1
            END DO
          END DO
        END DO
      !$OMP END PARALLEL
      IF ( ASSOCIATED(ptr) )  ERROR STOP 12

      !$OMP PARALLEL NUM_THREADS(Threads) SHARED(tgt1)
        DO CONCURRENT (I = 1:N, J = 1:10)
          DO CONCURRENT (K = 1:5)
            ptr => tgt1

            !$OMP PARALLEL PRIVATE(ptr) SHARED(tgt2)
              ptr => tgt2
            !$OMP END PARALLEL

            IF ( .NOT. ASSOCIATED(ptr, tgt1) )  ERROR STOP 13
          END DO
        END DO
      !$OMP END PARALLEL
      IF ( .NOT. ASSOCIATED(ptr, tgt1) )  ERROR STOP 14

      !$OMP PARALLEL NUM_THREADS(Threads) FIRSTPRIVATE(ptr) SHARED(tgt1)
        DO CONCURRENT (I = 1:N)
          DO CONCURRENT (J = 1:5)
            DO CONCURRENT (K = 1:5, L = 1:5)
              CALL Sub(ptr, tgt1)

              !$OMP PARALLEL PRIVATE(ptr) SHARED(tgt2)
              CALL Sub(ptr, tgt2)
              !$OMP END PARALLEL

              IF ( .NOT. ASSOCIATED(ptr, tgt1) )  ERROR STOP 15
            END DO
          END DO
        END DO
      !$OMP END PARALLEL
      IF ( .NOT. ASSOCIATED(ptr, tgt1) )  ERROR STOP 16

      CONTAINS

      PURE SUBROUTINE Sub(ptr, tgt)
      INTEGER, POINTER, INTENT(INOUT) :: ptr
      INTEGER, TARGET, INTENT(INOUT)  :: tgt
        ptr => tgt
      END SUBROUTINE
END
