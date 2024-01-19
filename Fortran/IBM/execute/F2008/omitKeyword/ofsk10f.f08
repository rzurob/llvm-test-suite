!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  For internal procedures, three subroutines are terminated by "END
!*  SUBROUTINE", and two other subroutine and one function are terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      PROGRAM fxip0203

      INTEGER  :: A(3,3) = RESHAPE( (/ 1,2,3,4,5,6,7,8,9 /), (/ 3,3 /) )

      INTERFACE

         FUNCTION ARRAYFCN1( )
            INTEGER ARRAYFCN1(3)
         END FUNCTION ARRAYFCN1

      END INTERFACE

C*---------------------------------------------------------------------
C*
C*    TEST: 001
C*
C*    Description: Testing An Array Valued Function as a actual
C*                 Argument to an Internal Procedure
C*
C*---------------------------------------------------------------------

      CALL SUB1( ARRAYFCN1( ) )

C*---------------------------------------------------------------------
C*
C*    TEST: 002
C*
C*    Description: Testing An Array Valued Internal Function as a actual
C*                 Argument to an Internal Procedure
C*
C*---------------------------------------------------------------------

      CALL SUB2( ARRAYFCN2( ) )

C*---------------------------------------------------------------------
C*
C*    TEST: 003
C*
C*    Description: Testing An Internal Function Which Calls An
C*                 Array Valued Internal Function
C*
C*---------------------------------------------------------------------

      CALL SUB3( )

C*---------------------------------------------------------------------
C*
C*    TEST: 004
C*
C*    Description: Testing An Array Valued Function as an Assumed
C*                 Shape actual Argument to an Internal Procedure
C*
C*---------------------------------------------------------------------

      CALL SUB4( ARRAYFCN2( ) )

C*---------------------------------------------------------------------
C*
C*    TEST: 005
C*
C*    Description: Testing An Array Valued Function as an Assumed
C*                 Size actual Argument to an Internal Procedure
C*
C*---------------------------------------------------------------------
      CALL SUB5( ARRAYFCN2( ) )

      CONTAINS

      SUBROUTINE SUB1( Array )
         INTEGER Array(3)
         IF (
     +   ( Array(1) .NE. 1 ) .OR.
     +   ( Array(2) .NE. 2 ) .OR.
     +   ( Array(3) .NE. 3 ) )
     +   ERROR STOP 1
      END SUBROUTINE

      SUBROUTINE SUB2( Array )
         INTEGER Array(3)
         IF (
     +   ( Array(1) .NE. 1 ) .OR.
     +   ( Array(2) .NE. 2 ) .OR.
     +   ( Array(3) .NE. 3 ) )
     +   ERROR STOP 2
      END SUBROUTINE

      SUBROUTINE SUB3( )
         INTEGER Array(3)
         Array = ARRAYFCN2( )
         IF (
     +   ( Array(1) .NE. 1 ) .OR.
     +   ( Array(2) .NE. 2 ) .OR.
     +   ( Array(3) .NE. 3 ) )
     +   ERROR STOP 3
      END

      SUBROUTINE SUB4( Array )
         INTEGER Array(-2:)
         IF (
     +   ( Array(-2) .NE. 1 ) .OR.
     +   ( Array(-1) .NE. 2 ) .OR.
     +   ( Array(0) .NE. 3 ) )
     +   ERROR STOP 4
         IF (
     +   ( LBOUND(Array,1) .NE. -2 ) .OR.
     +   ( UBOUND(Array,1) .NE. 0 ) )
     +   ERROR STOP 4
      END SUBROUTINE

      SUBROUTINE SUB5( Array )
         INTEGER Array(*)
         IF (
     +   ( Array(1) .NE. 1 ) .OR.
     +   ( Array(2) .NE. 2 ) .OR.
     +   ( Array(3) .NE. 3 ) )
     +   ERROR STOP 5

         IF (
     +   ( LBOUND(Array,1) .NE. 1 ) )
     +   ERROR STOP 5
      END

      FUNCTION ARRAYFCN2( )
         INTEGER ARRAYFCN2(3)
         INTEGER :: B(3) = (/ 1,2,3 /)
         ARRAYFCN2 = B
      END

      END

      FUNCTION ARRAYFCN1( )
         INTEGER ARRAYFCN1(3)
         INTEGER :: B(3) = (/ 1,2,3 /)
         ARRAYFCN1 = B
      END FUNCTION ARRAYFCN1


