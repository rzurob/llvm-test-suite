!*  ===================================================================
!*
!*                               Attribute in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Base Object appears in a
!*                               Specification Expression in a Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Any Statement of the Scoping Unit is
!*                               executed but the Variable is *NOT* a
!*                               Pending I/O Storage Sequence Affector
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Omit One Condition -- Derived Type, implicit Attribute, matches
!*  Condition (1) only.
!*
!*  5.1.2.3  ASYNCHRONOUS Attribute
!*
!*  The base object of a variable shall have the ASYNCHRONOUS attribute in
!*  a scoping unit if:
!*
!*  (1) the variable appears in an executable statement or specification
!*      expression in that scoping unit and
!*
!*  (2) any statement of the scoping unit is executed while the variable is
!*      a pending I/O storage sequence affector (9.5.1.4)
!*
!*  7.1.6 Specification expression
!*
!*  A specification expression is an expression with limitations that make it
!*  suitable for use in specifications such as length type parameters (C501)
!*  and array bounds (R512, R513).
!*
!*  R729 specification-expr  is  scalar-int-expr
!*
!*  C710 (R729) The scalar-int-expr shall be a restricted expression.
!*
!*  A restricted expression is an expression in which each operation is
!*  intrinsic and each primary is
!*
!*      ...
!*
!*      (2) An object designator with a base object that is a dummy argument
!*          that has neither the OPTIONAL nor the INTENT (OUT) attribute,
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mLength
    IMPLICIT NONE

    TYPE tLength
        INTEGER :: arrayLength
    END TYPE tLength
END MODULE mLength


PROGRAM baseObjSpecExpr02
    USE mLength

    IMPLICIT NONE

    INTERFACE
        INTEGER FUNCTION WriteList(lgth, state)
            USE mLength

            IMPLICIT NONE

            TYPE(tLength), INTENT(IN) :: lgth
            CHARACTER(LEN = 5), INTENT(OUT) :: state
        END FUNCTION WriteList
    END INTERFACE


    INTEGER :: oStat
    INTEGER :: cStat

    CHARACTER(LEN = 5) :: statement
    CHARACTER(LEN = 256) :: errMsg

    TYPE(tLength), PARAMETER :: lengths = tLength( 10 )


    OPEN(IOSTAT=oStat, IOMSG=errMsg, ASYNCHRONOUS='yes', UNIT=9)
    IF (oStat /= 0) THEN
        WRITE(0, *) "OPEN(Asynchronous): <", oStat, "> ", errMsg
        ERROR STOP 1
    END IF


    oStat = WriteList(lengths, statement)


    CLOSE(9, IOMSG=errMsg, IOSTAT=cStat)
    IF (cStat <> 0) THEN
        WRITE(0, *) "CLOSE(): <", cStat, "> ", errMsg
        ERROR STOP 4

    !
    !  Handle WRITE()/WAIT() failure termination.
    !
    ELSE IF (oStat /= 0) THEN
        IF (statement == 'WRITE') THEN
            ERROR STOP 2
        ELSE
            ERROR STOP 3
        END IF
    END IF

END PROGRAM baseObjSpecExpr02


INTEGER FUNCTION WriteList(lgth, state)
    USE mLength

    IMPLICIT NONE

    INTERFACE
        INTEGER FUNCTION AsynchronousWrite(n, list, stmt)
            USE mLength

            IMPLICIT NONE

            TYPE(tLength), ASYNCHRONOUS, INTENT(IN) :: n
            INTEGER, DIMENSION( n%arrayLength ),&
                        &ASYNCHRONOUS, INTENT(IN) :: list
            CHARACTER(LEN = 5), INTENT(OUT) :: stmt
        END FUNCTION AsynchronousWrite
    END INTERFACE


    INTEGER i
    INTEGER oS

    CHARACTER(LEN = 5), INTENT(OUT) :: state

    TYPE(tLength), INTENT(IN) :: lgth


    !
    !  1)  The Variable "lgth%arrayLength" appears in a Specification
    !      Expression in the Scoping Unit.
    !
    INTEGER, DIMENSION( lgth%arrayLength ) :: list


    list = (/ (i, i = 1, lgth%arrayLength) /)
    oS = AsynchronousWrite(lgth, list, state)

    !
    !  2)  A statement of the Scoping Unit is executed while the variable
    !      "lgth%arrayLength" is a "Pending I/O Storage Sequence Affector".
    !
    !  ==> This condition no longer is .TRUE. as "lgth%arrayLength" is no
    !      longer a "Pending I/O Storage Sequence Affector".
    !
    list = (/ ((99 - i), i = 1, lgth%arrayLength) /)


    WriteList = oS
END FUNCTION WriteList


INTEGER FUNCTION AsynchronousWrite(n, list, stmt)
    USE mLength

    IMPLICIT NONE

    INTEGER i
    INTEGER stat

    CHARACTER(LEN = 5), INTENT(OUT) :: stmt
    CHARACTER(LEN = 256) :: msg

    TYPE(tLength), ASYNCHRONOUS, INTENT(IN) :: n
    INTEGER, DIMENSION( n%arrayLength ), ASYNCHRONOUS, INTENT(IN) :: list


    !
    !  On successful completion of the Asynchronous WRITE(), the Variable
    !  "lgth%arrayLength" is a "Pending I/O Storage Sequence Affector".
    !
    WRITE(9, FMT=125, IOSTAT=stat, IOMSG=msg, ASYNCHRONOUS='yes')&
                    &n%arrayLength, (list( i ), i = 1, n%arrayLength)
125 FORMAT(11(I3))

    IF (stat <> 0) THEN
        stmt = 'WRITE'
        WRITE(0, *) "WRITE(): <", stat, "> ", msg
    END IF


    !
    !  On successful completion of the WAIT(), the variable "lgth%arrayLength"
    !  is no longer a "Pending I/O Storage Sequence Affector".
    !
    WAIT(9, IOSTAT=stat, IOMSG=msg)
    IF (stat <> 0) THEN
        stmt = 'WAIT'
        WRITE(0, *) "WAIT(): <", stat, "> ", msg
    END IF


    AsynchronousWrite = stat
END FUNCTION AsynchronousWrite
