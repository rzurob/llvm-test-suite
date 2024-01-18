! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-24
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Block with OMP
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!*  See defect 385736
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM BNestedTask
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 100, P = N**3
    INTEGER(8)         :: i, iMod
    INTEGER(8)         :: FncVar(P), localVar(P), temp1(P)

    i = P
    temp1 = 1

    !$OMP PARALLEL private (iMod) shared(temp1,localVar, FncVar) firstprivate(i)
    !$OMP master
     DO while (i > 0)
        iMod = mod(i, int(N,8))

        if (iMod == 0) iMod = N

        !$OMP task                                      !<-- localVar is shared; iMod is firstprivate
            BLOCK
               INTEGER(8) :: j =-99

                 !$OMP task shared(temp1)               !<-- j should be firstprivate here
                    DO j = 2, iMod
                        temp1(j) = j * temp1(j-1)
                    END DO
                 !$OMP END task

                 !$OMP critical (test)
                     IF ( j .NE. -99 ) STOP 10
                 !$OMP END critical (test)
            END BLOCK
            localVar(i) = temp1(iMod)
        !$OMP END task

        !$OMP task                                      !<-- FncVar is shared; iMod is firstprivate
            FncVar(i) = factorial(iMod)
        !$OMP END task

        i = i - 1
    END DO
    !$OMP END master
    !$OMP END PARALLEL

    !$OMP PARALLEL DO
        DO i = 1, size(FncVar)
           if (localVar(i) /= FncVar(i)) then
               print *, i, localVar(i), FncVar(i)
               call zzrcSyncd(int(i,4))
           END if
        END DO
    !$OMP END PARALLEL DO

    CONTAINS

     RECURSIVE INTEGER(8) FUNCTION factorial (i)
          INTEGER(8), intent(in) :: i

          if (i <= 1) then
              factorial = 1
          else
              !$OMP task shared(factorial) firstprivate (i)
              factorial = i * factorial(i-1)
              !$OMP END task
              !$OMP taskwait
          END if
      END FUNCTION

      SUBROUTINE zzrcSyncd(rc)
        INTEGER(4) :: rc
        !$OMP critical (termination)
          call zzrc(rc)
        !$OMP END critical (termination)
      END subroutine zzrcSyncd
END PROGRAM BNestedTask
