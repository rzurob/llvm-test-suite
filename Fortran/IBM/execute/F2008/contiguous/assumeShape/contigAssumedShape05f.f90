! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : CopyInOut5.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2011-08-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Copy-in/out for assumed shape arrays
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : 
!*       
!*                      
!*    - Actual argument is pointer array component of a basic derived type
!*    - Actual argument non-contiguous 
!*    - Dummy argument has contiguous attribute 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod
      IMPLICIT NONE
      INTEGER, PARAMETER :: N=100, N2=34  

      TYPE :: Base 
         INTEGER :: id = -1 
         CHARACTER(N) :: name = "XLFtest"
         INTEGER, POINTER :: ptr(:) => NULL()
      END TYPE

      CONTAINS

      SUBROUTINE Sub1(Arg)           
        INTEGER, CONTIGUOUS :: Arg(:)
        INTEGER :: I  

        IF (       .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 20
        IF ( SIZE(Arg) .NE.              N2 ) ERROR STOP 21
        IF ( ANY(Arg   .NE. [(I, I=1,N,3)]) ) ERROR STOP 22

        CALL SubSub(Arg) 

      END SUBROUTINE Sub1

      SUBROUTINE SubSub(Arg)
        INTEGER :: Arg(N2), I 

        IF (      .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 23
        IF ( ANY(Arg  .NE. [(I, I=1,N,3)]) ) ERROR STOP 24

        DO I = 1, N2
          IF ( Arg(I)     .NE.   (I-1)*3+1 ) ERROR STOP 25 
          CALL verify(I, Arg(I)) 
        ENDDO
      END SUBROUTINE SubSub

      SUBROUTINE verify(n, arg)           
        INTEGER, INTENT(IN) :: n, arg 

        IF ( arg .NE. (n-1)*3+1 ) ERROR STOP 26
      END SUBROUTINE verify

      INTEGER FUNCTION Fun(arg) 
        INTEGER, INTENT(IN) :: arg
 
        Fun = arg 
      END FUNCTION


END MODULE Mod
PROGRAM CopyInOut5
      USE Mod
      IMPLICIT NONE
      INTEGER         :: I
      INTEGER, TARGET :: tgt(N) = -99 
      TYPE(Base) :: b1
      CLASS(Base), POINTER :: b_ptr

      b1%ptr => tgt 
      IF ( b1%id        /=        -1 ) ERROR STOP 10
      IF ( b1%name      /= 'XLFtest' ) ERROR STOP 11
      IF ( size(b1%ptr) /=         N ) ERROR STOP 12
      IF ( any(b1%ptr   /=      -99) ) ERROR STOP 13


      DO I = 1, N 
         b1%ptr(I) = Fun(I) 
      ENDDO
      CALL Sub1(b1%ptr(1:N:3))
      
      ALLOCATE(b_ptr, SOURCE = b1) 
      CALL Sub1(b_ptr%ptr(1:N:3))
END PROGRAM CopyInOut5
