!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October  2, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Extended Derived Type (with Type Parameters)
!*                               Output to a SEQUENTIAL File
!*  SECONDARY FUNCTIONS TESTED : Output is performed by External Procedures
!*                               referenced via an INTERFACE (Procedure
!*                               selected via both Derived Type and KIND
!*                               Parameter values)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To Write an array of Derived Type through an External FUNCTION defined
!*  via an INTERFACE (based on Derived Type and KIND).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tMod
    IMPLICIT NONE

    TYPE tCmpxInt(k)
        INTEGER, KIND :: k

        INTEGER(k) :: r, i
    END TYPE tCmpxInt

END MODULE tMod


MODULE tEMod1
    USE tMod

    IMPLICIT NONE

    TYPE, EXTENDS(tCmpxInt) :: tCmpxIntLable1(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: lable
    END TYPE tCmpxIntLable1

END MODULE tEMod1


MODULE tEMod2
    USE tMod

    IMPLICIT NONE

    TYPE, EXTENDS(tCmpxInt) :: tCmpxIntLable2(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: lable
    END TYPE tCmpxIntLable2

END MODULE tEMod2

