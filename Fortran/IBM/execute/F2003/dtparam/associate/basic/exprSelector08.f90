!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : exprSelector08
!*
!*  DATE                       : August 15, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an expr
!*  SECONDARY FUNCTIONS TESTED : The Expression is a FUNCTION that Returns a
!*                               Procedure Pointer
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  * An expr that contains a:
!*    - Single primary (a FUNCTION Result that is a Procedure Pointer)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE modTPod
    IMPLICIT NONE

    TYPE tPod(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k
    END TYPE tPod


    ABSTRACT INTERFACE
        FUNCTION abstractNewPod(x, rc)
            IMPORT tPod

            CLASS(tPod(*,4)) :: x
            INTEGER(4) :: rc

            CLASS(tPod(:,4)), POINTER :: abstractNewPod
        END FUNCTION abstractNewPod
    END INTERFACE


    CONTAINS

        FUNCTION newTPod(x, rc)
            CLASS(tPod(*,4)) :: x
            INTEGER(4) :: rc

            CLASS(tPod(:,4)), POINTER :: newTPod

            SELECT TYPE ( x )
                TYPE IS (tPod(*,4))
                    ALLOCATE(newTPod, SOURCE=x)

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION newTPod

END MODULE modTPod

MODULE modTExtPod
    USE modTPod

    IMPLICIT NONE

    TYPE, EXTENDS(tPod) :: tExtPod
    END TYPE tExtPod


    CONTAINS

        FUNCTION newTExtPod(x, rc)
            CLASS(tPod(*,4)) :: x
            INTEGER(4) :: rc

            CLASS(tPod(:,4)), POINTER :: newTExtPod

            SELECT TYPE ( x )
                TYPE IS (tExtPod(*,4))
                    ALLOCATE(newTExtPod, SOURCE=x)

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION newTExtPod

END MODULE modTExtPod

PROGRAM exprSelector08
    USE modTExtPod

    IMPLICIT NONE


    TYPE(tPod(1,4)), TARGET :: podVar
    TYPE(tPod(:,4)), POINTER :: tPodPtr
    CLASS(tPod(:,4)), POINTER :: cPodPtr

    TYPE(tExtPod(3,4)), TARGET :: extPodVar
    TYPE(tExtPod(:,4)), POINTER :: tExtPodPtr
    CLASS(tExtPod(:,4)), POINTER :: cExtPodPtr

    PROCEDURE(abstractNewPod), POINTER :: newPodFunc, func


    cPodPtr => podVar
    cExtPodPtr => extPodVar

    newPodFunc => podFunc(cPodPtr, 10_4)
    tPodPtr => newPodFunc(cPodPtr, 20_4)

    IF (.NOT. ASSOCIATED( tPodPtr )) THEN
        CALL zzrc( 30_4 )

    ELSE IF (tPodPtr%k /= 4) THEN
        CALL zzrc( 40_4 )

    ELSE IF (tPodPtr%l /= 1) THEN
        CALL zzrc( 50_4 )
    END IF

    DEALLOCATE( tPodPtr )


!    ASSOCIATE(func => podFunc(cPodPtr, 110_4))
    func => podFunc(cPodPtr, 110_4)

        tPodPtr => func(cPodPtr, 120_4)
        IF (.NOT. ASSOCIATED( tPodPtr )) THEN
            CALL zzrc( 130_4 )

        ELSE IF (tPodPtr%k /= 4) THEN
            CALL zzrc( 140_4 )

        ELSE IF (tPodPtr%l /= 1) THEN
            CALL zzrc( 150_4 )
        END IF

!    END ASSOCIATE

    DEALLOCATE( tPodPtr )


!    ASSOCIATE(func => podFunc(cExtPodPtr, 210_4))
    func => podFunc(cExtPodPtr, 210_4)

    select type (x => func(cExtPodPtr, 220_4))
      type is (tExtPod(*,4))
!        tExtPodPtr => func(cExtPodPtr, 220_4)
        tExtPodPtr => x
        IF (.NOT. ASSOCIATED( tExtPodPtr )) THEN
            CALL zzrc( 230_4 )

        ELSE IF (tExtPodPtr%k /= 4) THEN
            CALL zzrc( 240_4 )

        ELSE IF (tExtPodPtr%l /= 3) THEN
            CALL zzrc( 250_4 )
        END IF
      class default
        stop 100

    end select
!    END ASSOCIATE

    DEALLOCATE( tExtPodPtr )


    CONTAINS

        FUNCTION podFunc(x, rc)
            CLASS(tPod(*,4)), INTENT(in) :: x
            INTEGER(4) :: rc

            PROCEDURE(abstractNewPod), POINTER :: podFunc

            SELECT TYPE ( x )
                TYPE IS (tPod(*,4))
                    podFunc => newTPod

                TYPE IS (tExtPod(*,4))
                    podFunc => newTExtPod

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION podFunc

END PROGRAM exprSelector08
