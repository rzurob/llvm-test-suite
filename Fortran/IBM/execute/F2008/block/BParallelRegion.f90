! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : BParallelRegion.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2011-01-24
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Block with OMP
!*                             :
!*  SECONDARY FUNCTIONS TESTED :  
!*                                
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                :  
!*
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
PROGRAM BParallelRegion
    use omp_lib
    IMPLICIT NONE
    INTEGER         :: i
    INTEGER         :: arr(4)

    i = -99 
    arr = 0   

    !$OMP PARALLEL num_threads(4) 
        BLOCK 
          INTEGER :: i 
            i = omp_get_thread_num()
            arr(i) = i 
        END BLOCK 
    !$OMP END PARALLEL

     call simple_sort(arr)
     IF ( ANY(arr .NE.  [3,2,1,0] ) ) STOP 10

     
    CONTAINS

    SUBROUTINE simple_sort (X)
      INTEGER :: X(:)
      INTEGER I, Iswap, temp

      DO I = 1,size(X)-1
         Iswap = MAXLOC(X(I:size(X)),1) + I-1
         IF(Iswap .NE. I) THEN
            TEMP = X(I)
            X(I) = X(Iswap)
            X(Iswap) = TEMP
         ENDIF
      END DO 
      END SUBROUTINE simple_sort

END PROGRAM BParallelRegion
