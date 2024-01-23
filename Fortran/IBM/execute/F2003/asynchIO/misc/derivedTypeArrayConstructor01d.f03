!*  ===================================================================
!*
!*                               Attribute in the ASSOCIATE Construct
!*
!*  DATE                       : March  1, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : associate-name => selector (where selector
!*                               implicitly has the ASYNCHRONOUS Attribute)
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, ASSOCIATE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Diagnostic Test Case.  Compilation fails during the initialization
!*  of a large ( >4990 Elements) array of Derived Type using an Implied
!*  DO Loop with a Structure Constructor.
!*
!*  A similar Implied DO Loop on an array of INTEGER is successful.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mType
    TYPE tType
        INTEGER :: iType
    END TYPE tType
END MODULE mType


PROGRAM derivedTypeArrayConstructor01d
    USE mType

    IMPLICIT NONE

    INTEGER, PARAMETER :: n = 10000
!    INTEGER, PARAMETER :: n = 4990

    INTEGER :: i
    INTEGER :: oStat

    CHARACTER(LEN = 256) :: oMsg

    INTEGER, DIMENSION( n ) :: thatType = (/ (i, i = 1, n) /)
    TYPE(tType), DIMENSION( n ) :: thisType = (/ (tType(i), i = 1, n) /)


    DO i = 1, n
        WRITE(*, FMT='(I5)', IOSTAT=oStat, IOMSG=oMsg)&
                                thisType( ((n - i) + 1) )%iType

        IF (oStat <> 0) THEN
            WRITE(0, *) i, ") WRITE(*) <", oStat, "> ", oMsg
            ERROR STOP 1
        END IF


        WRITE(0, FMT='(I5)',&
            &IOSTAT=oStat, IOMSG=oMsg) thatType( ((n - i) + 1) )
        IF (oStat <> 0) THEN
            WRITE(0, *) i, ") WRITE(*) <", oStat, "> ", oMsg
            ERROR STOP 2
        END IF
    END DO

END PROGRAM derivedTypeArrayConstructor01d
