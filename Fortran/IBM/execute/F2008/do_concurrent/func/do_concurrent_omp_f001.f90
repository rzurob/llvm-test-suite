!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 12, 2015
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT
!*  SECONDARY FUNCTIONS TESTED : OMP
!*
!*  REQUIRED COMPILER OPTIONS  : -qsmp
!*
!*  DESCRIPTION                : Test DO CONCURRENT statement and
!*                               construct in WORKSHARE and PARALLEL
!*                               WORKSHARE constructs.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM MAIN
      IMPLICIT NONE

      LOGICAL, EXTERNAL :: PRECISION_R8
      REAL*8 :: AA(500), BB(500), CC(500)
      INTEGER*8 :: I, J, K, L

      CALL OMP_SET_NUM_THREADS(4)
      CALL RANDOM_NUMBER(HARVEST = AA)
      AA = 10_8 * AA
      BB = AA
      CC = AA

      DO CONCURRENT (I = 1:500, BB(I) > 5.0)
        BB(I) = 1.0
      END DO

      DO CONCURRENT (I = 1:500, BB(I) < 2.0)
        BB(I) = 0.0
      END DO

      DO CONCURRENT (I = 1:500, BB(I) > 3.0)
        BB(I) = 0.5
      END DO

!$OMP WORKSHARE
      DO CONCURRENT (I = 1:500, CC(I) > 5.0)
        CC(I) = 1.0
      END DO

      DO CONCURRENT (I = 1:500, CC(I) < 2.0)
        CC(I) = 0.0
      END DO

      DO CONCURRENT (I = 1:500, CC(I) > 3.0)
        CC(I) = 0.5
      END DO
!$OMP END WORKSHARE

      IF (.NOT. PRECISION_R8(BB,CC)) ERROR STOP 1

      CC = AA

!$OMP PARALLEL
!$OMP WORKSHARE
      DO CONCURRENT (I = 1:500, CC(I) > 5.0)
        CC(I) = 1.0
      END DO

      DO CONCURRENT (I = 1:500, CC(I) < 2.0)
        CC(I) = 0.0
      END DO

      DO CONCURRENT (I = 1:500, CC(I) > 3.0)
        CC(I) = 0.5
      END DO
!$OMP END WORKSHARE
!$OMP END PARALLEL

      IF (.NOT. PRECISION_R8(BB,CC)) ERROR STOP 2

      CC = AA

!$OMP PARALLEL WORKSHARE
      DO CONCURRENT (I = 1:500, CC(I) > 5.0)
        CC(I) = 1.0
      END DO

      DO CONCURRENT (I = 1:500, CC(I) < 2.0)
        CC(I) = 0.0
      END DO

      DO CONCURRENT (I = 1:500, CC(I) > 3.0)
        CC(I) = 0.5
      END DO
!$OMP END PARALLEL WORKSHARE

      IF (.NOT. PRECISION_R8(BB,CC)) ERROR STOP 3

      DO CONCURRENT (I = 1:500, BB(I) > 5.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            BB(I) = 1.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, BB(I) < 2.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            BB(I) = 0.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, BB(I) > 3.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            BB(I) = 0.5
          END DO
        END DO
      END DO

!$OMP WORKSHARE
      DO CONCURRENT (I = 1:500, CC(I) > 5.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 1.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, CC(I) < 2.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 0.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, CC(I) > 3.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 0.5
          END DO
        END DO
      END DO
!$OMP END WORKSHARE

      IF (.NOT. PRECISION_R8(BB,CC)) ERROR STOP 4

      CC = AA

!$OMP PARALLEL
!$OMP WORKSHARE
      DO CONCURRENT (I = 1:500, CC(I) > 5.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 1.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, CC(I) < 2.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 0.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, CC(I) > 3.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 0.5
          END DO
        END DO
      END DO
!$OMP END WORKSHARE
!$OMP END PARALLEL

      IF (.NOT. PRECISION_R8(BB,CC)) ERROR STOP 5

      CC = AA

!$OMP PARALLEL WORKSHARE
      DO CONCURRENT (I = 1:500, CC(I) > 5.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K= 1:5, L = 1:5)
            CC(I) = 1.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, CC(I) < 2.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 0.0
          END DO
        END DO
      END DO

      DO CONCURRENT (I = 1:500, CC(I) > 3.0)
        DO CONCURRENT (J = 1:10)
          DO CONCURRENT (K = 1:5, L = 1:5)
            CC(I) = 0.5
          END DO
        END DO
      END DO
!$OMP END PARALLEL WORKSHARE

      IF (.NOT. PRECISION_R8(BB,CC)) ERROR STOP 6

      END
