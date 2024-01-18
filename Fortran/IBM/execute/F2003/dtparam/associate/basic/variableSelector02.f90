!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : variableSelector02
!*  TEST CASE TITLE            : variable selector with Derived Type Parameters
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July  7, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : the variable is of a Derived Type with
!*                               multiple (more than 2) Type Parameters
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
!*     Parameters (multiple KIND/LEN Parameters)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM variableSelector02
    IMPLICIT NONE

    TYPE dt_lkl(l1, k1, l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1
        INTEGER, LEN :: l2
    END TYPE dt_lkl

    TYPE dt_lkll(l1, k1, l2, l3)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1
        INTEGER, LEN :: l2
        INTEGER, LEN :: l3
    END TYPE dt_lkll

    TYPE dt_lkllk(l1, k1, l2, l3, k2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1
        INTEGER, LEN :: l2
        INTEGER, LEN :: l3
        INTEGER, KIND :: k2
    END TYPE dt_lkllk

    TYPE(dt_lkl(7,8,9)) :: dt_lkl_789
    TYPE(dt_lkll(3,4,5,7)) :: dt_lkll_3457
    TYPE(dt_lkllk(7,8,3,5,4)) :: dt_lkllk_78354


    PRINT *, "dt_lkl_789%l1 =", dt_lkl_789%l1,&
           ", k1 =", dt_lkl_789%k1,&
           ", l2 =", dt_lkl_789%l2

    ASSOCIATE(x => dt_lkl_789)
        PRINT *, "x%l1 =", x%l1,&
               ", k1 =", x%k1,&
               ", l2 =", x%l2

        IF (x%l1 /= dt_lkl_789%l1) THEN
            STOP 10

        ELSE IF (x%k1 /= dt_lkl_789%k1) THEN
            STOP 11

        ELSE IF (x%l2 /= dt_lkl_789%l2) THEN
            STOP 12

        ELSE IF (x%l1 /= 7) THEN
            STOP 13

        ELSE IF (x%k1 /= 8) THEN
            STOP 14

        ELSE IF (x%l2 /= 9) THEN
            STOP 15
        END IF
    END ASSOCIATE


    PRINT *, "dt_lkll_3457%l1 =", dt_lkll_3457%l1,&
           ", k1 =", dt_lkll_3457%k1,&
           ", l2 =", dt_lkll_3457%l2,&
           ", l3 =", dt_lkll_3457%l3

    ASSOCIATE(x => dt_lkll_3457)
        PRINT *, "x%l1 =", x%l1,&
               ", k1 =", x%k1,&
               ", l2 =", x%l2,&
               ", l3 =", x%l3

        IF (x%l1 /= dt_lkll_3457%l1) THEN
            STOP 20

        ELSE IF (x%k1 /= dt_lkll_3457%k1) THEN
            STOP 21

        ELSE IF (x%l2 /= dt_lkll_3457%l2) THEN
            STOP 22

        ELSE IF (x%l3 /= dt_lkll_3457%l3) THEN
            STOP 23

        ELSE IF (x%l1 /= 3) THEN
            STOP 24

        ELSE IF (x%k1 /= 4) THEN
            STOP 25

        ELSE IF (x%l2 /= 5) THEN
            STOP 26

        ELSE IF (x%l3 /= 7) THEN
            STOP 27
        END IF
    END ASSOCIATE


    PRINT *, "dt_lkllk_78354%l1 =", dt_lkllk_78354%l1,&
           ", k1 =", dt_lkllk_78354%k1,&
           ", l2 =", dt_lkllk_78354%l2,&
           ", l3 =", dt_lkllk_78354%l3,&
           ", k2 =", dt_lkllk_78354%k2

    ASSOCIATE(x => dt_lkllk_78354)
        PRINT *, "x%l1 =", x%l1,&
               ", k1 =", x%k1,&
               ", l2 =", x%l2,&
               ", l3 =", x%l3,&
               ", k2 =", x%k2

        IF (x%l1 /= dt_lkllk_78354%l1) THEN
            STOP 20

        ELSE IF (x%k1 /= dt_lkllk_78354%k1) THEN
            STOP 21

        ELSE IF (x%l2 /= dt_lkllk_78354%l2) THEN
            STOP 22

        ELSE IF (x%l3 /= dt_lkllk_78354%l3) THEN
            STOP 23

        ELSE IF (x%k2 /= dt_lkllk_78354%k2) THEN
            STOP 24

        ELSE IF (x%l1 /= 7) THEN
            STOP 25

        ELSE IF (x%k1 /= 8) THEN
            STOP 26

        ELSE IF (x%l2 /= 3) THEN
            STOP 27

        ELSE IF (x%l3 /= 5) THEN
            STOP 28

        ELSE IF (x%k2 /= 4) THEN
            STOP 29
        END IF
    END ASSOCIATE

END PROGRAM variableSelector02
