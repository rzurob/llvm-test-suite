!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : May 21, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Default Initialization with Structure
!*                               Constructors
!*  SECONDARY FUNCTIONS TESTED : Generic Function References
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  From the F2003 Stanard (pg 64):
!*
!*  NOTE 4.56
!*  The form 'name(...)' is interpreted as a generic function-reference if
!*  possible; it is interpreted as a structure-constructor only if it cannot
!*  be interpreted as a generic function-reference.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mBase
    IMPLICIT NONE

    TYPE tBase(kBase,lBase)
        INTEGER, KIND :: kBase = 4
        INTEGER, LEN :: lBase = 3

        INTEGER :: kVal = kBase
        INTEGER :: lVal = kBase
    END TYPE tBase

    CONTAINS

        FUNCTION BaseInit(kVal, lVal)
            INTEGER :: kVal
            INTEGER :: lVal

            TYPE(tBase(4,lVal)) :: BaseInit

            PRINT *, "BaseInit(", kVal, ",", lVal, ")"
            BaseInit%kval = 4
            BaseInit%lval = 4

        END FUNCTION BaseInit

END MODULE mBase


MODULE mExt
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tExt
        CLASS(tBase(kBase,lBase)), POINTER :: next => NULL( )
    END TYPE tExt

END MODULE mExt


MODULE mCheck
    USE mExt

    IMPLICIT NONE

    CONTAINS

        INTEGER(4) FUNCTION CheckBase(base, kBase, lBase, kVal, lVal, rc)
            TYPE(tBase(4,*)) :: base
            INTEGER :: kBase
            INTEGER :: lBase
            INTEGER :: kVal
            INTEGER :: lVal
            INTEGER(4) :: rc


            PRINT *, "CheckBase(", kBase, ",", lBase, ",",&
                                    kVal, ",", lVal, ",", rc, ")"

            CheckBase = 0_4
            IF (base%kBase /= kBase) THEN
                CheckBase = rc

            ELSE IF (base%kBase /= kBase) THEN
                CheckBase = rc + 1_4

            ELSE IF (base%lBase /= lBase) THEN
                CheckBase = rc + 2_4

            ELSE IF (base%kVal /= kVal) THEN
                CheckBase = rc + 3_4

            ELSE IF (base%lVal /= lVal) THEN
                CheckBase = rc + 4_4
            END IF

        END FUNCTION CheckBase


        INTEGER(4) FUNCTION CheckExt(ext, kBase, lBase, kVal, lVal, next, rc)
            TYPE(tExt(4,*)) :: ext
            INTEGER :: kBase
            INTEGER :: lBase
            INTEGER :: kVal
            INTEGER :: lVal
            CLASS(tBase(4,*)), TARGET :: next
            INTEGER(4) :: rc


            PRINT *, "CheckExt(", kBase, ",", lBase, ",",&
                                    kVal, ",", lVal, ",", rc, ")"

            CheckExt = CheckBase(ext%tBase, kBase, lBase, kVal, lVal, rc)
            IF (CheckExt == 0_4) THEN
                IF (.NOT. ASSOCIATED( ext%next ) ) THEN
                    CheckExt = rc + 6_4

                ELSE IF (.NOT. ASSOCIATED( ext%next, next) ) THEN
                    CheckExt = rc + 7_4
                END IF
            END IF

        END FUNCTION CheckExt

END MODULE mCheck


PROGRAM dtpDefaultInit01
    USE mBase
    USE mExt
    USE mCheck

    IMPLICIT NONE

    INTERFACE tBase
        MODULE PROCEDURE BaseInit
    END INTERFACE tBase

    INTERFACE tExt
        FUNCTION ExtInit(kVal, lVal, next)
            IMPORT tBase, tExt
            INTEGER :: kVal
            INTEGER :: lVal
            CLASS(tBase(4,*)), TARGET :: next

            TYPE(tExt(4,lVal)) :: ExtInit
        END FUNCTION ExtInit
    END INTERFACE tExt

    INTEGER(4) :: rc
    TYPE(tBase(4,:)), ALLOCATABLE, TARGET :: base


    base = tBase(4,3)(4,3)
    rc = CheckBase(base, 4, 3, 4, 3, 10_4)


    IF (rc == 0_4) THEN
        base = tBase(4, 3)
        rc = CheckBase(base, 4, 3, 4, 4, 20_4)
    END IF


    IF (rc == 0_4) THEN
        base = tBase()()
        rc = CheckBase(base, 4, 3, 4, 4, 30_4)
    END IF


    IF (rc == 0_4) THEN
        rc = InitExt(base, 100_4)
    END IF


    CALL zzrc( rc )


    CONTAINS

        INTEGER(4) FUNCTION InitExt(theBase, rc)
            TYPE(tBase(4,*)), TARGET :: theBase
            INTEGER(4) :: rc

            TYPE(tExt(4,:)), POINTER :: ext


            ALLOCATE(tExt(4,3) :: ext)

            ext = tExt(lVal=3, kVal=4, next=theBase)
            InitExt = CheckExt(ext, 4, 3, 4, 3, theBase, rc)


            IF (InitExt == 0_4) THEN
                ext = tExt()(next=base)
                InitExt = CheckExt(ext, 4, 3, 4, 4, base, (rc + 10_4))
            END IF


            IF (InitExt == 0_4) THEN
                ext = tExt(lBase=3,kBase=4)(lVal=3,kVal=4,next=ext)
                InitExt = CheckExt(ext, 4, 3, 4, 3, ext, (rc + 20_4))
            END IF


            IF ( ASSOCIATED( ext ) ) THEN
                DEALLOCATE( ext )
            END IF

        END FUNCTION InitExt

END PROGRAM dtpDefaultInit01


FUNCTION ExtInit(kVal, lVal, next)
    USE mExt

    IMPLICIT NONE

    INTEGER :: kVal
    INTEGER :: lVal
    CLASS(tBase(4,*)), TARGET :: next

    TYPE(tExt(4,lVal)) :: ExtInit


    PRINT *, "ExtInit(", kVal, ",", lVal, ")"

    ExtInit%tBase = tBase(lVal=lVal, kVal=kVal)
    ExtInit%next => next

END FUNCTION ExtInit
