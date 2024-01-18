!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpStructConstr03
!*
!*  DATE                       : May 27, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Structure Constructor
!*  SECONDARY FUNCTIONS TESTED : with POINTER and PROCEDURE POINTER Components
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mInit
    IMPLICIT NONE

    INTEGER, PRIVATE :: i

    REAL(16), SAVE, TARGET :: staticTarget( 10 ) =&
                        [ ((1.0_16 / REAL(i, 16)), i = 1, 10) ]
    REAL(16), TARGET :: allocatableTarget( 10 ) =&
                        [ ((1.0_16 / REAL((i + 10), 16)), i = 1, 10) ]
    REAL(16), TARGET :: moduleTarget( 10 ) =&
                        [ ((1.0_16 / REAL((i + 20), 16)), i = 1, 10) ]

END MODULE mInit


MODULE mBase
    USE mInit

    IMPLICIT NONE

    TYPE tBase(kBase,lBase)
        INTEGER, KIND :: kBase
        INTEGER, LEN :: lBase

        REAL(kBase), POINTER :: data( : )

        PROCEDURE(ProcBaseInit), POINTER, PASS :: myInit

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual16
            PROCEDURE, PASS :: NotEqual16 => TBase16NotEqual

    END TYPE tBase

    INTERFACE
        SUBROUTINE ProcBaseInit( this )
            IMPORT tBase
            CLASS(tBase(16,*)), INTENT(inout) :: this
        END SUBROUTINE ProcBaseInit
    END INTERFACE

    CONTAINS

        LOGICAL FUNCTION TBase16NotEqual(this, that)
            CLASS(tBase(16,*)), INTENT(in) :: this
            CLASS(tBase(16,*)), INTENT(in) :: that

            TBase16NotEqual = .TRUE.
            IF ((this%kBase == that%kBase)                  .AND.&
                (this%lBase == that%lBase)                  .AND.&
                ( ASSOCIATED(this%myInit, that%myInit) )    .AND.&
                ( ASSOCIATED( this%data ) )                 .AND.&
                ( ASSOCIATED( that%data ) )                 .AND.&
                (SIZE( this%data ) == SIZE( that%data ))    .AND.&
                ( ALL(this%data == that%data) )) THEN
                TBase16NotEqual = .FALSE.

            ELSE
                PRINT *,"this%kBase =",this%kBase,", that%kBase =",that%kBase
                PRINT *,"this%lBase =",this%lBase,", that%lBase =",that%lBase
                PRINT *, "ASSOCIATED(this%myInit, that%myInit) =",&
                                ASSOCIATED(this%myInit, that%myInit)
                PRINT *, "ASSOCIATED( this%data ) =", ASSOCIATED( this%data ),&
                        ", ASSOCIATED( that%data ) =", ASSOCIATED( that%data )
                PRINT *, "SIZE( this%data ) =", SIZE( this%data ),&
                        ", SIZE( that%data ) =", SIZE( that%data )
                PRINT *, "this%data = [", this%data, "]"
                PRINT *, "that%data = [", that%data, "]"
            END IF

        END FUNCTION TBase16NotEqual

        SUBROUTINE BaseInitStatic( this )
            CLASS(tBase(16,*)), INTENT(inout) :: this

            INTEGER :: i
            REAL(16), SAVE, TARGET :: staticData( 10 ) = 0.0_16

            IF (staticData( 1 ) == 0.0_16) staticData = staticTarget

            this%data => NULL( )
            IF (this%lBase <= SIZE( staticData )) THEN
                this%data => staticData( :this%lBase )
            END IF

        END SUBROUTINE BaseInitStatic

        SUBROUTINE BaseInitAllocatable( this )
            CLASS(tBase(16,*)), INTENT(inout) :: this

            REAL(16), ALLOCATABLE, TARGET, SAVE :: realArray( : )

            IF (.NOT. ALLOCATED( realArray )) THEN
                ALLOCATE(realArray( SIZE( allocatableTarget ) ),&
                                            SOURCE=allocatableTarget)
            END IF

            this%data => NULL( )
            IF (this%lBase <= SIZE( allocatableTarget )) THEN
                this%data => realArray( :this%lBase )
            END IF

        END SUBROUTINE BaseInitAllocatable

        SUBROUTINE BaseInitModule( this )
            CLASS(tBase(16,*)), INTENT(inout) :: this

            this%data => NULL( )
            IF (this%lBase <= SIZE( moduleTarget )) THEN
                this%data => moduleTarget( :this%lBase )
            END IF

        END SUBROUTINE BaseInitModule

END MODULE mBase


PROGRAM dtpStructConstr03
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE TestStatic(n, rc)
            INTEGER :: n
            INTEGER(4) :: rc
        END SUBROUTINE TestStatic

        SUBROUTINE TestAllocatable(n, rc)
            INTEGER :: n
            INTEGER(4) :: rc
        END SUBROUTINE TestAllocatable

        SUBROUTINE TestModule(n, rc)
            INTEGER :: n
            INTEGER(4) :: rc
        END SUBROUTINE TestModule
    END INTERFACE

    INTEGER :: i
    INTEGER(4) :: rcBase

    DO i = 1, 10
        rcBase = INT((i - 1), 4)

        CALL TestStatic(i, (10_4 + (rcBase * 5_4)))
        CALL TestAllocatable(i, (100_4 + (rcBase * 10_4)))
        CALL TestModule(i, (200_4 + (rcBase * 5_4)))
    END DO

END PROGRAM dtpStructConstr03


SUBROUTINE TestStatic(n, rc)
    USE mBase

    IMPLICIT NONE

    INTEGER :: n
    INTEGER(4) :: rc

    TYPE(tBase(16,:)), ALLOCATABLE :: staticProc
    TYPE(tBase(16,:)), ALLOCATABLE :: staticConstr
    TYPE(tBase(16,:)), ALLOCATABLE :: staticVerify


    staticProc = tBase(16,n)(NULL( ),BaseInitStatic)
    CALL staticProc%myInit( )

    staticConstr = tBase(16,n)(staticTarget( :n ),BaseInitStatic)

    ALLOCATE(tBase(16,n) :: staticVerify)
    staticVerify%data => staticTarget( :staticVerify%lBase )
    staticVerify%myInit => BaseInitStatic


    IF (staticProc /= staticVerify)     CALL zzrc( rc )
    IF (staticConstr /= staticVerify)   CALL zzrc( (rc + 1_4) )

END SUBROUTINE TestStatic


SUBROUTINE TestAllocatable(n, rc)
    USE mBase

    IMPLICIT NONE

    INTEGER :: n
    INTEGER(4) :: rc

    TYPE(tBase(16,n)), TARGET :: allocatableProc
    TYPE(tBase(16,:)), POINTER :: ptrAllocatableProc

    TYPE(tBase(16,n)), TARGET :: allocatableConstr
    TYPE(tBase(16,:)), POINTER :: ptrAllocatableConstr

    TYPE(tBase(16,n)), TARGET :: allocatableVerify
    TYPE(tBase(16,:)), POINTER :: ptrAllocatableVerify


    ptrAllocatableProc => allocatableProc
    ptrAllocatableProc =&
        tBase(16,ptrAllocatableProc%lBase)(NULL( ),BaseInitAllocatable)
    CALL ptrAllocatableProc%myInit( )


    ptrAllocatableConstr => allocatableConstr
    ptrAllocatableConstr =&
        tBase(16,n)(allocatableTarget( :n ),BaseInitAllocatable)


    ptrAllocatableVerify => allocatableVerify
    ptrAllocatableVerify%data => allocatableTarget( :allocatableVerify%lBase )
    ptrAllocatableVerify%myInit => BaseInitAllocatable


    IF (ptrAllocatableVerify /= allocatableVerify)      CALL zzrc( rc )

    IF (allocatableProc /= allocatableVerify)           CALL zzrc( (rc + 1_4) )
    IF (ptrAllocatableProc /= ptrAllocatableVerify)     CALL zzrc( (rc + 2_4) )
    IF (ptrAllocatableProc /= allocatableProc)          CALL zzrc( (rc + 3_4) )

    IF (allocatableConstr /= allocatableVerify)         CALL zzrc( (rc + 4_4) )
    IF (ptrAllocatableConstr /= ptrAllocatableVerify)   CALL zzrc( (rc + 5_4) )
    IF (ptrAllocatableConstr /= allocatableConstr)      CALL zzrc( (rc + 6_4) )

END SUBROUTINE TestAllocatable


SUBROUTINE TestModule(n, rc)
    USE mBase

    IMPLICIT NONE

    INTEGER :: n
    INTEGER(4) :: rc

    TYPE(tBase(16,n)) :: moduleProc
    TYPE(tBase(16,n)) :: moduleConstr
    TYPE(tBase(16,n)) :: moduleVerify


    moduleProc = tBase(16,n)(NULL( ),BaseInitModule)
    CALL moduleProc%myInit( )

    moduleConstr =&
        tBase(16,moduleConstr%lBase)(&
                moduleTarget( :moduleConstr%lBase ),BaseInitModule)

    moduleVerify%data => moduleTarget( :n )
    moduleVerify%myInit => BaseInitModule


    IF (moduleProc /= moduleVerify)     CALL zzrc( rc )
    IF (moduleConstr /= moduleVerify)   CALL zzrc( (rc + 1_4) )

END SUBROUTINE TestModule
