!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSaveAttr01 - ASYNCHRONOUS Attribute
!*                               Interactions with Other Attributes
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the SAVE Attribute
!*
!*  DRIVER STANZA              : xlf2003
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
!*  R502 declaration-type-spec  is  intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or  SAVE
!*
!*
!*  5.1.2.13 SAVE attribute
!*
!*  An entity with the SAVE attribute, in the scoping unit of a subprogram,
!*  retains its association status, allocation status, definition status,
!*  and value after execution of a RETURN or END statement unless it is a
!*  pointer and its target becomes undefined (16.4.2.1.3(4)). It is shared
!*  by all instances (12.5.2.3) of the subprogram.
!*
!*  An entity with the SAVE attribute, declared in the scoping unit of a
!*  module, retains its association status, allocation status, definition
!*  status, and value after a RETURN or END statement is executed in a
!*  procedure that accesses the module unless it is a pointer and its
!*  target becomes undefined.
!*
!*
!*  5.2.12 SAVE statement
!*
!*  R543 save-stmt          is  SAVE [ [ :: ] saved-entity-list ]
!*  R544 saved-entity       is  object-name
!*                          or  proc-pointer-name
!*                          or  / common-block-name /
!*  R545 proc-pointer-name  is  name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSaveAttr01
    IMPLICIT NONE


    INTERFACE
        INTEGER FUNCTION UpdateAsynchCounter( ioUnit )
            IMPLICIT NONE
            INTEGER :: ioUnit
        END FUNCTION UpdateAsynchCounter

        INTEGER FUNCTION UpdateSynchCounter( ioUnit )
            IMPLICIT NONE
            INTEGER :: ioUnit
        END FUNCTION UpdateSynchCounter
    END INTERFACE


    INTEGER :: oStat
    INTEGER :: wStat
    INTEGER :: ioUnit = 39

    CHARACTER(LEN = 256) :: oMsg


    OPEN(UNIT=ioUnit, ASYNCHRONOUS='yes', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    wStat = UpdateSynchCounter( ioUnit )
    IF (wStat == 0) THEN
        wStat = UpdateAsynchCounter( ioUnit )
    END IF


    CLOSE(UNIT=ioUnit, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 3 )

    ELSE IF (wStat <> 0) THEN
        CALL zzrc( 2 )
    END IF

END PROGRAM asynchSaveAttr01


INTEGER FUNCTION UpdateSynchCounter( ioUnit )
    IMPLICIT NONE

    INTEGER :: oStat
    INTEGER :: ioUnit

    INTEGER, SAVE :: counter = 0

    CHARACTER(LEN = 256) :: oMsg


    counter = counter + 1
    WRITE(UNIT=ioUnit, FMT=20, IOSTAT=oStat, IOMSG=oMsg) counter
20  FORMAT(' counter = "',I3,'"')

    IF (oStat <> 0) THEN
        WRITE(0, *) "WRITE() <", oStat, "> ", oMsg
    END IF

    UpdateSynchCounter = oStat
END FUNCTION UpdateSynchCounter


INTEGER FUNCTION UpdateAsynchCounter( ioUnit )
    IMPLICIT NONE

    INTEGER :: oStat
    INTEGER :: ioUnit

    INTEGER, ASYNCHRONOUS, SAVE :: aCounter = 0

    CHARACTER(LEN = 256) :: oMsg


    aCounter = aCounter + 1
    WRITE(UNIT=ioUnit, FMT=20,&
            &ASYNCHRONOUS='yes', IOSTAT=oStat, IOMSG=oMsg) aCounter
20  FORMAT('aCounter = "',I3,'"')

    IF (oStat <> 0) THEN
        WRITE(0, *) "WRITE(Asynchronous) <", oStat, "> ", oMsg
    END IF

    UpdateAsynchCounter = oStat
END FUNCTION UpdateAsynchCounter
