! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/asynchIO/attribute/baseObj/specExprPending03.f
! opt variations: -ql -qreuse=self

!*  ===================================================================
!*
!*                               Attribute in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Base Object appears in a
!*                               Specification Expression in a Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Any Statement of the Scoping Unit is
!*                               executed while the Variable is a Pending
!*                               I/O Storage Sequence Affector
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Baseline Test Case #3 -- Derived Type, explicit Attribute, matches
!*  both conditions.
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
!*  A specification expression is an expression with limitations that make
!*  it suitable for use in specifications such as length type parameters
!*  (C501) and array bounds (R512, R513).
!*
!*  R729 specification-expr  is  scalar-int-expr
!*
!*  C710 (R729) The scalar-int-expr shall be a restricted expression.
!*
!*  A restricted expression is an expression in which each operation is
!*  intrinsic and each primary is
!*
!*      ....
!*
!*      (4) An object designator with a base object that is made accessible
!*          by use association or host association,
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPixel
    IMPLICIT NONE

    TYPE tPixel(K1,K2,K3,K4,K5)    ! (1,1,1,4,4)
        INTEGER, KIND :: K1,K2,K3,K4,K5
        INTEGER(K1)   :: red
        INTEGER(K2)   :: green
        INTEGER(K3)   :: blue

        REAL(K4)      :: x
        REAL(K5)      :: y
    END TYPE tPixel

    TYPE(tPixel(1,1,1,4,4)), ASYNCHRONOUS :: asynchPixel =&
                  tPixel(1,1,1,4,4)(233,198,201, 43.289,15.124)
END MODULE mPixel


PROGRAM specExprPending03
    USE mPixel

    IMPLICIT NONE

    INTEGER :: ioStatus

    TYPE(tPixel(1,1,1,4,4)) :: startPixel


    !
    !  1) The variable "asynchPixel" appears in a "Specification Expression"
    !     in the Scoping Unit.
    !
    !  Same colour, different location.
    !
    startPixel = tPixel(1,1,1,4,4)(asynchPixel%red,&
                        &asynchPixel%green,&
                        &asynchPixel%blue,&
                        &12.236, 27.118)


    OPEN(10, IOSTAT=ioStatus, ERR=200, ASYNCHRONOUS='yes')


    CALL AsynchronousWrite( )


    !
    !  2)  A statement of the Scoping Unit is executed while the variable
    !      "asynchPixel" is a "Pending I/O Storage Sequence Affector".
    !
    PRINT *, "Start Pixel = (", startPixel%red, ",",&
                                &startPixel%green, ",",&
                                &startPixel%blue, ","
    PRINT *, "               ", startPixel%x, ",",&
                                &startPixel%y, ")"


    CLOSE(10, IOSTAT=ioStatus, ERR=300)
    GO TO 100


    !
    !  Report CLOSE() Error.
    !
300 CONTINUE
    PRINT *, "CLOSE():  I/O Status = (", ioStatus, ")"
    ERROR STOP 3
    GO TO 100


    !
    !  Report OPEN() Error.
    !
200 CONTINUE
    PRINT *, "OPEN():  I/O Status = (", ioStatus, ")"
    ERROR STOP 1


100 CONTINUE

END PROGRAM specExprPending03


SUBROUTINE AsynchronousWrite( )
    USE mPixel

    INTEGER :: iStatus

    IMPLICIT NONE


    !
    !  On completion of the WRITE() Statement, the Structure "asynchPixel"
    !  becomes a "Pending I/O Storage Sequence Affector".
    !
    WRITE(10, 100, ASYNCHRONOUS='yes', IOSTAT=iStatus, ERR=200)&
                                    &asynchPixel%x, asynchPixel%y
    GOTO 300

100 FORMAT("Asynchronous Pixel (x,y) = (",f6.3,",",f6.3,")")


    !
    !  Report WRITE() Error.
    !
200 CONTINUE
    PRINT *, "WRITE():  I/O Status = (", iStatus, ")"
    ERROR STOP 2


300 CONTINUE

END SUBROUTINE AsynchronousWrite
