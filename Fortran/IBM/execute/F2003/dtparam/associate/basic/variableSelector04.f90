!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July  8, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : variable is a Polymorphic Extended Type
!*                               (with Type Parameters)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  A Polymorphic variable of a Derived Type that requires Type Parameters
!*     (an Extended Derived Type)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM variableSelector04
    IMPLICIT NONE

    TYPE base(l1, k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: array( l1 )
    END TYPE base

    TYPE, EXTENDS(base) :: extended(l2)
        INTEGER, LEN :: l2

        INTEGER(k1) :: extArray( l2 )
    END TYPE extended


    INTEGER :: i
    INTEGER(4) :: iSum = 0
    REAL(4) :: rSum = 0.0_8

    CLASS(extended(:,4,:)), ALLOCATABLE :: extended


    ALLOCATE(extended,&
             SOURCE=extended(3,4,5)([ 1.0_4, 2.0_4, 3.0_4 ],&
                                    [ 1_4, -2_4, 3_4, -4_4, 5_4 ]))

    ASSOCIATE(pE => extended)

        DO i = 1, SIZE( pE%array )
            rSum = rSum + pE%array( i )
        END DO


        IF (SIZE( pE%array ) /= pE%l1) THEN
            STOP 10

        ELSE IF (KIND( pE%array ) /= pE%k1) THEN
            STOP 20

        ELSE IF (SIZE( pE%array ) /= 3) THEN
            STOP 30

        ELSE IF (KIND( pE%array ) /= 4) THEN
            STOP 40
        END IF

        SELECT TYPE ( pE )
            TYPE IS (extended(*,4,*))
                DO i = 1, pE%l2
                    iSum = iSum + pE%extArray( i )
                END DO

                IF (SIZE( pE%extArray ) /= 5) THEN
                    STOP 50

                ELSE IF (SIZE( pE%extArray ) /= pE%l2) THEN
                    STOP 60

                ELSE IF (KIND( pE%extArray ) /= pE%k1) THEN
                    STOP 70

                ELSE IF (KIND( pE%extArray ) /= 4) THEN
                    STOP 80
                END IF

            CLASS DEFAULT
                STOP 90
        END SELECT
    END ASSOCIATE


    IF (rSum /= 6.0_4) THEN
        STOP 100

    ELSE IF (iSum /= 3_4) THEN
        STOP 110
    END IF

END PROGRAM variableSelector04
