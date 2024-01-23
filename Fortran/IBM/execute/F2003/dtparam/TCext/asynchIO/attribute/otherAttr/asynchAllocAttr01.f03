! GB DTP extension using:
! ftcx_dtp -qdeferredlp /tstdev/F2003/asynchIO/attribute/otherAttr/asynchAllocAttr01.f
! opt variations: -qck -qnodeferredlp

!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the ALLOCATABLE Attribute
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl-list
!*
!*  R502 declaration-type-spec is intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*
!*  5.1.2.2 ALLOCATABLE attribute
!*
!*  An object with the ALLOCATABLE attribute is one for which space is
!*  allocated by an ALLOCATE statement (6.3.1) or by an intrinsic assignment
!*  statement (7.4.1.3).
!*
!*  5.2.2 ALLOCATABLE statement
!*
!*  R520 allocatable-stmt is ALLOCATABLE [ :: ]&
!*                           &object-name [ (deferred-shape-spec-list) ]&
!*                           &[, object-name [ (deferred-shape-spec-list) ]] ...
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchAllocAttr01
    IMPLICIT NONE

    TYPE tNameRec(K1,N1,N2)    ! (4,10,1)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1,N2
        INTEGER(K1)   :: id
        CHARACTER(N1) :: name
        CHARACTER(N2) :: newLine
    END TYPE tNameRec

    TYPE(tNameRec(4,:,:)), ASYNCHRONOUS, ALLOCATABLE :: aNameRecord
    TYPE(tNameRec(4,:,:)), ASYNCHRONOUS, ALLOCATABLE, DIMENSION( : ) :: nameList

    INTEGER :: i
    INTEGER :: oStatus
    INTEGER :: iStatus
    INTEGER :: wStatus
    INTEGER :: cStatus

    INTEGER, PARAMETER :: n = 10
    INTEGER, DIMENSION( n ) :: idList

    CHARACTER(LEN = 256) :: ioErrMsg


    ALLOCATE(tNameRec(4,10,1) :: nameList( n ), STAT=iStatus, ERRMSG=ioErrMsg)
    IF (iStatus <> 0) THEN
        WRITE(0, *) "ALLOCATE(): <", iStatus, "> ", ioErrMsg
        ERROR STOP 1
    END IF



    OPEN(UNIT=2600, FILE="asynchAllocAttr01.dat", RECL=15,&
        &ASYNCHRONOUS='yes', ACCESS='direct', ACTION='read',&
            &IOSTAT=oStatus, IOMSG=ioErrMsg, FORM='formatted')
    IF (oStatus <> 0) THEN
        WRITE(0, *) "OPEN(): <", oStatus, "> ", ioErrMsg
        ERROR STOP 2
    END IF


    i = 0
    iStatus = 0

    DO WHILE ((iStatus .EQ. 0)  .AND. (i .LT. n))
        i = i + 1

        READ(2600, 100, REC=i, ASYNCHRONOUS='yes', IOSTAT=iStatus,&
                        &IOMSG=ioErrMsg, ID=idList( i )) nameList( i )
    END DO


    IF (iStatus <> 0) THEN
        WRITE(0, *) "READ() <", iStatus, "> ", ioErrMsg

    ELSE
        i = 0
        wStatus = 0

        DO WHILE ((wStatus .EQ. 0)  .AND. (i .LT. n))
            i = i + 1

            WAIT(2600, ID=idList( i ), IOSTAT=wStatus, IOMSG=ioErrMsg)
        END DO

        IF (wStatus <> 0) THEN
            WRITE(0, *) "WAIT() <", wStatus, "> ", ioErrMsg
        END IF
    END IF


    CLOSE(2600, IOSTAT=cStatus, IOMSG=ioErrMsg)
    IF (cStatus <> 0) THEN
        WRITE(0, *) "CLOSE() <", cStatus, "> ", ioErrMsg
        ERROR STOP 5


    ELSE IF (wStatus <> 0) THEN
        ERROR STOP 4

    ELSE IF (iStatus <> 0) THEN
        ERROR STOP 3
    END IF


    PRINT 200
    DO i = 1, n
        PRINT 300, nameList( i )%id, nameList( i )%name
    END DO


100 FORMAT(i4,a10,a)
200 FORMAT("ID  Name List")
300 FORMAT(i2,'  ',a10)


END PROGRAM asynchAllocAttr01
