!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : variableSelector01
!*  TEST CASE TITLE            : variable selector with Derived Type Parameters
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July  7, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : variable is a Derived Type (with Type
!*                               Parameters)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  A variable declared to be of a Derived Type that requires Type
!*     Parameters (KIND, LEN, and both)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM variableSelector01
    IMPLICIT NONE

    TYPE dt_l( l )
        INTEGER, LEN :: l
    END TYPE dt_l

    TYPE dt_k( k )
        INTEGER, KIND :: k
    END TYPE dt_k

    TYPE dt_lk(l, k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k
    END TYPE dt_lk

    TYPE(dt_l(13)) :: dt_l_13
    TYPE(dt_k(16)) :: dt_k_16
    TYPE(dt_lk(7,8)) :: dt_lk_7_8


    PRINT *, "dt_l_13%l =", dt_l_13%l
    ASSOCIATE(x => dt_l_13)
        PRINT *, "x%l =", x%l

        IF (x%l /= dt_l_13%l) THEN
            STOP 10

        ELSE IF (x%l /= 13) THEN
            STOP 11
        END IF
    END ASSOCIATE


    PRINT *, "dt_k_16%k =", dt_k_16%k
    ASSOCIATE(x => dt_k_16)
        PRINT *, "x%k =", x%k

        IF (x%k /= dt_k_16%k) THEN
            STOP 20

        ELSE IF (x%k /= 16) THEN
            STOP 21
        END IF
    END ASSOCIATE


    PRINT *, "dt_lk_7_8%l =", dt_lk_7_8%l, ", dt_lk_7_8%k =", dt_lk_7_8%k
    ASSOCIATE(x => dt_lk_7_8)
        PRINT *, "x%l =", x%l, ", x%k =", x%k

        IF (x%l /= dt_lk_7_8%l) THEN
            STOP 30

        ELSE IF (x%k /= dt_lk_7_8%k) THEN
            STOP 31

        ELSE IF (x%l /= 7) THEN
            STOP 32

        ELSE IF (x%k /= 8) THEN
            STOP 33
        END IF
    END ASSOCIATE

END PROGRAM variableSelector01
