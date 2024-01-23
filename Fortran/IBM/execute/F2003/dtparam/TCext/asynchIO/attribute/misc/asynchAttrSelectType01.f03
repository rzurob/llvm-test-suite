! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/asynchIO/attribute/misc/asynchAttrSelectType01.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Attribute in the SELECT TYPE Construct
!*
!*  DATE                       : March  2, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : associate-name => selector (where selector
!*                               implicitly has the ASYNCHRONOUS Attribute)
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  8.1.4.3 Attributes of associate names
!*
!*  Within a SELECT TYPE or ASSOCIATE construct, ... The associating entity
!*  has the ASYNCHRONOUS, TARGET, or VOLATILE attribute if and only if the
!*  selector is a variable and has the attribute.
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mBase
    TYPE tBase(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        INTEGER(K1)   :: baseInt1
    END TYPE tBase
END MODULE mBase


MODULE mDerived1
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived1    ! (20,4)
        INTEGER(K1) :: derivedInt1
    END TYPE tDerived1
END MODULE mDerived1


PROGRAM asynchAttrSelectType01
    USE mDerived1

    INTERFACE
        SUBROUTINE Process( anObj )
            USE mDerived1
            CLASS(tBase(*,4)), INTENT(IN) :: anObj
        END SUBROUTINE Process
    END INTERFACE


    CHARACTER(LEN = 256) :: oMsg

    TYPE(tBase(:,4)), ALLOCATABLE, DIMENSION( : ) :: base
    TYPE(tDerived1(:,4)), ALLOCATABLE, DIMENSION( : ) :: derived

    OPEN(102, ACCESS='stream', ACTION='write',&
        &ASYNCHRONOUS='yes', FORM='formatted',IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", oMsg
        ERROR STOP 1
    END IF


    ALLOCATE(tBase(20,4) :: base( 1000 ), STAT=iStat, ERRMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "ALLOCATE( tBase(1000) ) <", iStat, "> ", oMsg
        ERROR STOP 2
    END IF

    DO i = 1, 1000
        base( i )%baseInt1 = i
    END DO


    ALLOCATE(tDerived1(20,4) :: derived( 1000 ), STAT=iStat, ERRMSG=oMsg)

    IF (iStat == 0) THEN
        DO i = 1, 1000
            derived( i )%baseInt1 = i
            derived( i )%derivedInt1 = 1000 - i
        END DO
    END IF


    IF (iStat <> 0) THEN
        WRITE(0, *) "ALLOCATE( tDerived(1000) ) <", iStat, "> ", oMsg
        ERROR STOP 3
    END IF


    DO i = 1, 1000
        CALL Process( derived( i ) )
        CALL Process( base( (1000 - i + 1) ) )
    END DO


    CLOSE(102, IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", oMsg
        ERROR STOP 5
    END IF

END PROGRAM asynchAttrSelectType01


SUBROUTINE Process( anObj )
    USE mDerived1

    CLASS(tBase(*,4)), INTENT(IN) :: anObj

    INTEGER :: iStat
    CHARACTER(LEN = 256) :: oMsg


    WRITE(102, FMT='(I5)', ASYNCHRONOUS='yes',&
            &IOSTAT=iStat, IOMSG=oMsg) anObj%baseInt1
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE(Asynchronous) <", iStat, "> ", oMsg
        ERROR STOP 4
    END IF


    SELECT TYPE (thisObj => anObj)
        TYPE IS (tBase(*,4))
            WRITE(102, FMT=100, IOSTAT=iStat, IOMSG=oMsg)&
                                    &'tBase', thisObj%baseInt1
            IF (iStat <> 0) THEN
                WRITE(0, *) "WRITE() <", iStat, "> ", oMsg
                ERROR STOP 5
            END IF

        CLASS IS (tBase(*,4))
            WRITE(102, FMT=200, IOSTAT=iStat, IOMSG=oMsg)&
                                    &'tBase', thisObj%baseInt1
            IF (iStat <> 0) THEN
                WRITE(0, *) "WRITE() <", iStat, "> ", oMsg
                ERROR STOP 6
            END IF
    END SELECT


100 FORMAT('TYPE(',A9,'):   thisObj%baseInt1    = "',I5,'"')
200 FORMAT('CLASS(',A9,'):  thisObj%baseInt1    = "',I5,'"')

END SUBROUTINE Process
