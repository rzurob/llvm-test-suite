C***************************************************************************
C %START
C %MAIN: YES
C %PRECMD:
C %COMPOPTS:
C %GROUP: fxsf000p.f
C %VERIFY:
C %STDIN:
C %STDOUT:
C %EXECARGS:
C %POSTCMD:
C %END
C***************************************************************************
C***********************************************************************
C* =================================================================== *
C*                                                                     *
C* DATE                       : 25 October 1990                        *
C*                                                                     *
C* CHANGE HISTORY             : Original                               *
C*                                                                     *
C* NUMBER OF TESTS            : 9                                      *
C*                                                                     *
C* DESCRIPTION                : INT*4 STATEMENT FUNCTION with POINTER  *
C*                              arguments and expressions              *
C*                                                                     *
C* STRUCTURE                  : MAIN                                   *
C*                                                                     *
C* EXECUTABLE                 : Yes                                    *
C*                                                                     *
C* INPUTS                     : None                                   *
C*                                                                     *
C* OUTPUTS                    : None                                   *
C*                                                                     *
C* SETUP REQUIREMENTS         : N/A                                    *
C*                                                                     *
C* DEPENDENCIES               : External routine ZZRC                  *
C*                                                                     *
C* REQUIRED COMPILER OPTIONS  : None                                   *
C*                                                                     *
C* NORMAL COMPLETION          : Return code = 0                        *
C*                                                                     *
C* ABNORMAL COMPLETION        : Return code ^= 0                       *
C*                                                                     *
C* RUN TIME ESTIMATE          : <60 SEC                                *
C*                                                                     *
C* CONDITIONS TESTED          : Listed below.                          *
C/
C/ LAST TEST LIST UPDATE      : SYAU     25 October 1990               *
C/
C/  TEST   DESCRIPTION
C/  ----   ------------------------------------------------------------
C/  NOTE !!! : All variables and arrays (argument or expression) used are
C/             pointer based
C/
C/   1     Empty argument list, single POINTER variable as expression
C/   2     Empty argument list, LOC intrinsic function as expression
C/   3     Empty argument list, POINTER variables expression
C/
C/   4     pass single POINTER var., single POINTER variable as expression
C/   5     pass single POINTER var., LOC intrinsic function as expression
C/   6     pass single POINTER var., POINTER variables expression
C/
C/   7     pass multiple POINTER var., single POINTER variable as expression
C/   8     pass multiple POINTER var., LOC intrinsic function as expression
C/   9     pass multiple POINTER var., POINTER variables expression
C* =================================================================== *
C***********************************************************************

      PROGRAM FXSF000P
      use, intrinsic :: iso_fortran_env

      implicit none

C
C     VARIABLES STATEMENT FUNCTION ARGUMENTS
C
      INTEGER(INT32)   I4ARG1,I4ARG2,I4ARG_B1,I4ARG_B2

C     DEFINE POINTER BASED ARGUMENTS
      POINTER (I4ARG1P,I4ARG1), (I4ARG2P,I4ARG2)

C     POINTER VARIABLES USED IN STATEMENT FUNCTION EXPRESSIONS
      INTEGER(INT32) I4VAR1, I4VAR2, I4ARR(2)
      INTEGER(INT32) I4VAR_B1, I4VAR_B2, I4ARR_B(2)
      POINTER (I4VAR1P,I4VAR1), (I4VAR2P,I4VAR2), (I4ARRP,I4ARR)
      INTEGER(INT32) SF_RETURN, RESET          ! STATEMENT FUNCTION VALUES
      POINTER (P, SF_RETURN)
      PARAMETER (RESET=2147483647)

C
C     STATEMENT FUNCTIONS DEFINITION
C
      INTEGER(INT32) SF01,SF03, SF11,SF13, SF21,SF23
      INTEGER*8 SF02,SF12,SF22

C     STATEMENT FUNCTION WITH EMPTY ARGUMENT LIST
      SF01() = I4VAR1P                          ! POINTER variable
      SF02() = LOC(I4VAR2P)                     ! intrinsic function ref.
      SF03() = I4VAR1P*I4VAR2P                  ! POINTER variable expression

C     STATEMENT WITH SINGLE ARGUMENT
      SF11(I4ARG1P) = I4ARG1P                   ! POINTER variable
      SF12(I4ARG1P) = LOC(I4VAR1P)              ! intrinsic function reference
      SF13(I4ARG1P) = I4VAR1P*I4ARG1P           ! POINTER variable expression

C     STATEMENT FUNCTION WITH MULTIPE ARGUMENTS
      SF21(I4ARG1P,I4ARG2P) = I4ARG2P           ! POINTER variable
      SF22(I4ARG1P,I4ARG2P) = LOC(I4ARRP)         ! intrinsic function ref.
      SF23(I4ARG1P,I4ARG2P) = I4ARG1P*I4ARG2P+1 ! POINTER variable expression

C     RESET RETURN CODE
      ERROR STOP 0

C
C     TEST INVOCATION OF EACH STATEMENT FUNCTION
C

C     ARGUMENT LIST EMPTY

      P = RESET
      I4VAR1P = 127
      P = SF01()
      IF (P .NE. 127) ERROR STOP 1

      P = RESET
      P = SF02()
      IF (P .NE. LOC(I4VAR2P)) ERROR STOP 2

      P = RESET
      I4VAR2P=3
      P = SF03()
      IF (P .NE. 381) ERROR STOP 3

C     SINGLE ARGUMENT

      P = RESET
      I4ARG1P = -127
      P = SF11(I4ARG1P)
      IF (P .NE. -127) ERROR STOP 4

      P = RESET
      P = SF12(I4ARG1P)
      IF (P .NE. LOC(I4VAR1P)) ERROR STOP 5

      P = RESET
      I4ARG1P=6
      P = SF13(I4ARG1P)
      IF (P .NE. 762) ERROR STOP 6

C     MULTIPLE ARGUMENTS

      P = RESET
      I4ARG2P=10
      P = SF21(I4ARG1P, I4ARG2P)
      IF (P .NE. 10) ERROR STOP 7

      P = RESET
      P = SF22(I4ARG1P,I4ARG2P)
      IF (P .NE. LOC(I4ARRP)) ERROR STOP 8

      P = RESET
      P = SF23(I4ARG1P,I4ARG2P)
      IF (P .NE. 61) ERROR STOP 9

      END

