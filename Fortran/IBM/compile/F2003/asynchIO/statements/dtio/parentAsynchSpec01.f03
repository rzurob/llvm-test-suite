!*  ===================================================================
!*
!*                               (DTIO) Interactions with Asynchronous I/O
!*
!*  DATE                       : April 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted Derived-Type I/O (DTIO) Routines
!*                               Defined for a Derived-Type
!*  SECONDARY FUNCTIONS TESTED : ASYNCHRONOUS= Specifier in the Parent I/O
!*                               Statements; ID= Specifier a Parent I/O
!*                               Statement
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), ID= Specifier, ASYNCHRONOUS=
!*                               Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*
!*  9.5 Data transfer statements
!*
!*  R910 read-stmt        is  READ ( io-control-spec-list )[ input-item-list]
!*  R911 write-stmt       is  WRITE ( io-control-spec-list )[ output-item-list]
!*
!*  9.5.1 Control information list
!*
!*  R913 io-control-spec  is  [ UNIT = ] io-unit
!*                        or  [ FMT = ] format
!*                        or  [ NML = ] namelist-group-name
!*                        or  ADVANCE = scalar-default-char-expr
!*                        or  ASYNCHRONOUS = scalar-char-initialization-expr
!*  ...
!*                        or  ID = scalar-int-variable
!*
!*  9.5.1.4 ASYNCHRONOUS= specifier in a data transfer statement
!*
!*  The ASYNCHRONOUS= specifier determines whether this input/output
!*  statement is synchronous or asynchronous.
!*
!*  9.5.1.8 ID= specifier in a data transfer statement
!*
!*  Successful execution of an asynchronous data transfer statement
!*  containing an ID= specifier causes the variable specified in the
!*  ID= specifier to become defined with a processor-dependent value.
!*
!*  9.5.3.7.2 User-defined derived-type input/output procedures
!*
!*  Neither a parent nor child data transfer statement shall be asynchronous.
!*
!*  NOTE 9.48
!*  A child data transfer statement shall not specify the ID=, POS=, or
!*  REC= specifiers in an input/output control list.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mBase

    TYPE tBase
        INTEGER :: bInt
        REAL :: bReal
        COMPLEX :: bComp
    END TYPE tBase


    INTERFACE write(unformatted)
        SUBROUTINE TBaseWrite(obj, unit, iostat, iomsg)
            IMPORT tBase
            CLASS(tBase), INTENT(in) :: obj
            INTEGER, INTENT(in) :: unit
            INTEGER, INTENT(out) :: iostat
            CHARACTER(*), INTENT(inout) :: iomsg
        END SUBROUTINE TBaseWrite
    END INTERFACE

    INTERFACE read(unformatted)
        SUBROUTINE TBaseRead(obj, unit, iostat, iomsg)
            IMPORT tBase
            CLASS(tBase), INTENT(inout) :: obj
            INTEGER, INTENT(in) :: unit
            INTEGER, INTENT(out) :: iostat
            CHARACTER(*), INTENT(inout) :: iomsg
        END SUBROUTINE TBaseRead
    END INTERFACE

END MODULE mBase


PROGRAM parentAsynchSpec01
    USE mBase

    INTEGER :: anInt2
    INTEGER :: anInt1 = 1234

    REAL :: aReal2
    REAL :: aReal1 = 5.678

    COMPLEX :: aComp2
    COMPLEX :: aComp1 = (9.012,3.456)

    TYPE(tBase) :: baseObj2
    TYPE(tBase) :: baseObj1 = tBase(6543,2.109,(876.5,4.321))

    CHARACTER(LEN = 256) :: iMsg


    OPEN(3, ASYNCHRONOUS='yes', FORM='unformatted', IOMSG=iMsg,&
            ACCESS='sequential', ACTION='readwrite', IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    WRITE(3, ASYNCHRONOUS='yes',&
        IOSTAT=iStat, IOMSG=iMsg) aComp1, aReal1, anInt1
    IF (iStat /= 0) THEN
        WRITE(0, *) "WRITE(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF

    WRITE(3, ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg) baseObj1
    IF (iStat /= 0) THEN
        WRITE(0, *) "WRITE(ASYNCHRONOUS=yes) baseObj <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF


    REWIND(3, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "REWIND() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF


    READ(3, ASYNCHRONOUS='yes', ID=ioID, IOSTAT=iStat, IOMSG=iMsg) baseObj2
    IF (iStat /= 0) THEN
        WRITE(0, *) "READ(ASYNCHRONOUS=yes) baseObj <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF

    READ(3, ASYNCHRONOUS='yes',&
        IOSTAT=iStat, IOMSG=iMsg) aComp2, aReal2, anInt2
    IF (iStat /= 0) THEN
        WRITE(0, *) "READ(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF


    CLOSE(3, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 7 )
    END IF


    IF (aReal2 /= baseObj1%bReal) THEN
        WRITE(0, *) "aReal2 = '", aReal2,&
                    "', baseObj1%bReal = '", baseObj1%bReal, "'"
        CALL zzrc( 8 )

    ELSE IF (aComp2 /= baseObj1%bComp) THEN
        WRITE(0, *) "aComp2 = '", aComp2,&
                    "', baseObj1%bComp = '", baseObj1%bComp, "'"
        CALL zzrc( 9 )

    ELSE IF (anInt2 /= baseObj1%bInt) THEN
        WRITE(0, *) "aInt2 = '", aInt2,&
                    "', baseObj1%bInt = '", baseObj1%bInt, "'"
        CALL zzrc( 10 )

    ELSE IF (aReal1 /= baseObj2%bReal) THEN
        WRITE(0, *) "aReal1 = '", aReal1,&
                    "', baseObj2%bReal = '", baseObj2%bReal, "'"
        CALL zzrc( 11 )

    ELSE IF (aComp1 /= baseObj2%bComp) THEN
        WRITE(0, *) "aComp1 = '", aComp1,&
                    "', baseObj2%bComp = '", baseObj2%bComp, "'"
        CALL zzrc( 12 )

    ELSE IF (anInt1 /= baseObj2%bInt) THEN
        WRITE(0, *) "aInt1 = '", aInt1,&
                    "', baseObj2%bInt = '", baseObj2%bInt, "'"
        CALL zzrc( 13 )
    END IF

END PROGRAM parentAsynchSpec01


SUBROUTINE TBaseWrite(obj, unit, iostat, iomsg)
    USE mBase, ONLY : tBase

    CLASS(tBase), INTENT(in) :: obj
    INTEGER, INTENT(in) :: unit
    INTEGER, INTENT(out) :: iostat
    CHARACTER(*), INTENT(inout) :: iomsg


    WRITE( unit ) obj%bComp, obj%bReal, obj%bInt

END SUBROUTINE TBaseWrite


SUBROUTINE TBaseRead(obj, unit, iostat, iomsg)
    USE mBase, ONLY : tBase

    CLASS(tBase), INTENT(inout) :: obj
    INTEGER, INTENT(in) :: unit
    INTEGER, INTENT(out) :: iostat
    CHARACTER(*), INTENT(inout) :: iomsg


    READ( unit ) obj%bComp, obj%bReal, obj%bInt

END SUBROUTINE TBaseRead
