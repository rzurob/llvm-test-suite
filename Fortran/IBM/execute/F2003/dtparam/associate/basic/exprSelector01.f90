!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July  8, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an Expression
!*  SECONDARY FUNCTIONS TESTED : The Expression is a Structure Constructor
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  An expr that contains a:
!*     -  Single primary (a Structure Constructor)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM exprSelector01
    IMPLICIT NONE

    TYPE tK1(k1)
        INTEGER, KIND :: k1

        INTEGER(k1) :: k1Val = k1
    END TYPE tK1

    TYPE, EXTENDS(tK1) :: tK2(k2)
        INTEGER, KIND :: k2

        INTEGER(k2) :: k2Val = k2
    END TYPE tK2


    TYPE tL1(l1)
        INTEGER, LEN :: l1

        CHARACTER(l1) :: l1Val
    END TYPE tL1

    TYPE, EXTENDS(tL1) :: tL2(l2)
        INTEGER, LEN :: l2

        CHARACTER(l2) :: l2Val
    END TYPE tL2


    TYPE tKL(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        REAL(k) :: array( l,l )! = REAL(-1.0,k)
    END TYPE tKL


    INTEGER :: i
    INTEGER :: j
    REAL(4) :: prod = 1.0_4


    real :: array2(2,2)


    ASSOCIATE(struct => tK1(4)(4))
        IF (struct%k1Val /= 4_4) THEN
            STOP 10

        ELSE IF (KIND( struct%k1Val ) /= 4) THEN
            STOP 11
        END IF
    END ASSOCIATE

    ASSOCIATE(struct => tK2(4,8)(4,8))
        IF (struct%k1Val /= 4_4) THEN
            STOP 20

        ELSE IF (KIND( struct%k1Val ) /= 4) THEN
            STOP 21

        ELSE IF (struct%k2Val /= 8_8) THEN
            STOP 22

        ELSE IF (KIND( struct%k2Val ) /= 8) THEN
            STOP 23
        END IF
    END ASSOCIATE


    ASSOCIATE(struct => tL1(3)('IBM'))
        IF (struct%l1Val /= 'IBM') THEN
            STOP 30

        ELSE IF (LEN( struct%l1Val ) /= 3) THEN
            STOP 31
        END IF
    END ASSOCIATE

    ASSOCIATE(struct => tL2(5,5)('Hello','World'))
        IF (struct%l1Val /= 'Hello') THEN
            STOP 40

        ELSE IF (LEN( struct%l1Val ) /= 5) THEN
            STOP 41

        ELSE IF (struct%l2Val /= 'World') THEN
            STOP 42

        ELSE IF (LEN( struct%l2Val ) /= 5) THEN
            STOP 43
        END IF
    END ASSOCIATE


    array2 = 2.0

    call ASSOCIATE_replacer (tKL(4,2)(array2))

    contains

!    ASSOCIATE(struct => tKL(4,2)(array2))
    subroutine ASSOCIATE_replacer (struct)
        type(tkl(4,*)), intent(in) :: struct

        IF (KIND( struct%array ) /= 4) THEN
            STOP 50

        ELSE IF ( ANY( SHAPE( struct%array ) /= 2) ) THEN
            STOP 51
        END IF

        DO j = 1, SIZE(struct%array, 1)
            DO i = 1, SIZE(struct%array, 2)
                prod = prod * struct%array( j,i )
            END DO
        END DO

        PRINT *, "prod =", prod
        IF (prod /= 16.0_4) THEN
            STOP 52
        END IF
    END subroutine

END PROGRAM exprSelector01
