! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Recursive2.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : 2010-10-18
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*                             :
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - Recursive subroutine call with assumed 
!*                                   shape array where the shape changes at 
!*                                   each recursive call
!*                               - Actual argument is simply contiguous 
!*                               - Dummy argument is an assumed shape array with 
!*                                   CONTIGUOUS attribute 
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
PROGRAM Recursive2
      IMPLICIT NONE
      INTEGER :: I
      REAL(8) :: x
      CLASS(*), ALLOCATABLE :: a(:)
      LOGICAL, EXTERNAL :: precision_r8

      x = sum1([3._8,3._8,3._8]) 
      IF (.NOT. precision_r8(x, 9._8)) STOP 10

      x = sum1([(I*0.1_8, I=1,1024)]) 
      IF (.NOT. precision_r8(x, 52480._8)) STOP 11

      ALLOCATE( REAL(8) :: a(10) )

      SELECT TYPE (s => a)
          TYPEIS (real(4))
            STOP 12
          TYPEIS (real(8))
            s = -1._8
            x = sum1(s)
            IF (.NOT. precision_r8(x, -10._8)) STOP 13
        CLASS DEFAULT
            STOP 14
      END SELECT
      
      CONTAINS

      RECURSIVE REAL(8) FUNCTION sum1(r)
        REAL(8), CONTIGUOUS, INTENT(IN) :: r(:)
        INTEGER :: n

        n = size(r) 

        IF( n <= 1 ) then
            sum1 = r(1) 
            return 
        ELSE
            sum1 = sum1(r(1:n-1)) + r(n) 
        ENDIF
      END FUNCTION

END PROGRAM Recursive2
