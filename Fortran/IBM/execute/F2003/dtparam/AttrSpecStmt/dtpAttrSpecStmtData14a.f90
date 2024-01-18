!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtData14a
!*                               (based on dtpAttrSpecStmtData14)
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  -- DATA statement
!*
!*  A zero-sized array or a data-implied-do with an iteration count of
!*  zero contributes no variables to the expanded sequence of variables
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE m

    TYPE :: dt0(k0,l0)
        INTEGER, KIND :: k0 = 1
        INTEGER, LEN  :: l0 = 1
    END TYPE

    TYPE,  EXTENDS(dt0)  :: dt1(k1,l1)
        INTEGER(k0), KIND   :: k1 = 1
        INTEGER(k0), LEN    :: l1 = 1

        INTEGER(k1)         :: r( l1 )    ! = k1
    END TYPE

END MODULE m


PROGRAM dtpAttrSpecStmtData14a
    USE m

    TYPE(dt1(1,3,4,5)), PARAMETER :: C11 = dt1(1,3,4,5)(dt0=dt0(1,3)(),r=1)
    TYPE(dt1(1,3,4,5)), PARAMETER :: C12 = dt1(1,3,4,5)(dt0=dt0(1,3)(),r=2)
    TYPE(dt1(1,3,4,5)), PARAMETER :: C13 = dt1(1,3,4,5)(dt0=dt0(1,3)(),r=3)

    INTEGER, PARAMETER :: N = 31

    INTEGER :: i
    INTEGER :: j

    TYPE(dt1(1,3,4,5))     :: s1( N,N )


    DATA s1( :,1 ), s1( N:1,2 ), s1( :,3 )  / N * C11, N * C13 /


    DO i = 1, N
        PRINT *, i
        PRINT '(5(Z8.8," "))', s1( i,1 )%r, s1( i,2 )%r, s1( i,3 )%r

        IF ( ANY(s1( i,1 )%r /= C11%r) ) STOP 21
        IF ( ANY(s1( i,2 )%r == C12%r) ) STOP 22
        IF ( ANY(s1( i,3 )%r /= C13%r) ) STOP 23
    END DO

END PROGRAM dtpAttrSpecStmtData14a
