!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : d355425
!*
!*  DATE                       : August 26, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  ABSTRACT                   : ASSOCIATE: PROCPTR: RES: Unknown Data Type
!*                               for FUNCTION Return
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  ORIGINAL TEST CASE         :
!*  F2003/dtparam/associate/basic/exprSelector08.scenario
!*
!*  DESCRIPTION                :
!*  The ASSOCIATE Construct in the Reduced Code (below) uses the Procedure
!*  Pointer result from a FUNCTION as the selector.  When compiled, the
!*  compiler emits the following Diagnostic:
!*
!*  1517-001 (U) Data type is unknown.  Please contact your Service
!*  Representative.  For more information visit:
!*  http://www.ibm.com/support/docview.wss?uid=swg21110810
!*
!*  NOTE:  The Reduced Code for this Test Case *DOES NOT* use Derived
!*         Type Parameters.  The Original Test Case implements the
!*         DTP variation for this Defect.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE d355425mod
    IMPLICIT NONE

    TYPE tPod
    END TYPE tPod

    ABSTRACT INTERFACE
        FUNCTION abstractNewPod( x )
            IMPORT tPod
            CLASS(tPod) :: x
            CLASS(tPod), POINTER :: abstractNewPod
        END FUNCTION abstractNewPod
    END INTERFACE

    CONTAINS

        FUNCTION newTPod( x )
            CLASS(tPod) :: x
            CLASS(tPod), POINTER :: newTPod

            SELECT TYPE ( x )
                TYPE IS (tPod)
                    ALLOCATE(newTPod, SOURCE=x)
            END SELECT

        END FUNCTION newTPod

END MODULE d355425mod

PROGRAM d355425
    USE d355425mod

    IMPLICIT NONE

    TYPE(tPod), TARGET :: podVar
    TYPE(tPod), POINTER :: tPodPtr
    CLASS(tPod), POINTER :: cPodPtr
    PROCEDURE(abstractNewPod), POINTER :: newPodFunc

    cPodPtr => podVar

    newPodFunc => podFunc( cPodPtr )
    tPodPtr => newPodFunc( cPodPtr )

    IF (.NOT. ASSOCIATED( tPodPtr )) THEN
        STOP 10
    END IF

    DEALLOCATE( tPodPtr )

    ASSOCIATE(func => podFunc( cPodPtr ))   ! <= Line 46: Comment from here ...
        tPodPtr => func( cPodPtr )          !    Test Case PASSES if these

        IF (.NOT. ASSOCIATED( tPodPtr )) THEN
            STOP 20
        END IF

    END ASSOCIATE                           !    four lines are removed

    DEALLOCATE( tPodPtr )                   ! <= Line 49: ... Comment to here

    CONTAINS

        FUNCTION podFunc( x )
            CLASS(tPod), INTENT(in) :: x
            PROCEDURE(abstractNewPod), POINTER :: podFunc

            SELECT TYPE ( x )
                TYPE IS (tPod)
                    podFunc => newTPod
            END SELECT

        END FUNCTION podFunc

END PROGRAM d355425
