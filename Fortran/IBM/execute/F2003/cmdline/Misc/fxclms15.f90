! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov 1, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   	: COMMAND_ARGUMENT_COUNT()
!*                            	: GET_COMMAND(COMMAND, LENGTH, STATUS)
!*                            	: GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
!*                             	: GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 252525
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Invoke command_argument_count to initialize various entities
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclms15
      IMPLICIT NONE


      character(300)             :: CmdLine = 'fxclms15 -1 -2 -3 -4 -5'
      integer                    :: CmdCount = 5
      integer                    :: i
      INTEGER, ALLOCATABLE       :: A(:)
      INTEGER                    :: B(5)
      INTEGER                    :: C
      CHARACTER*(5)              :: D
      CHARACTER*(5)              :: E(5)
      COMPLEX                    :: F

      INTERFACE
        SUBROUTINE S(A, B, C, D, E, F)
          INTEGER, ALLOCATABLE                 :: A(:)
          INTEGER                              :: B(5)
          INTEGER                              :: C
          CHARACTER*(5)                        :: D
          CHARACTER*(5)                        :: E(5)
          COMPLEX                              :: F
        END SUBROUTINE
      END INTERFACE

      F = (COMMAND_ARGUMENT_COUNT(),COMMAND_ARGUMENT_COUNT())
      IF (F .ne. (5.0, 5.0) ) ERROR STOP 70

      CALL S(A, B, C=COMMAND_ARGUMENT_COUNT(), D=D, E=E, F=(COMMAND_ARGUMENT_COUNT(),COMMAND_ARGUMENT_COUNT()))


      END


      SUBROUTINE S(A, B, C, D, E, F)


      character(300)                       :: CmdLine = 'fxclms15 -1 -2 -3 -4 -5'
      integer                              :: CmdCount = 5
      integer                              :: i
      INTEGER, ALLOCATABLE                 :: A(:)
      INTEGER                              :: B(COMMAND_ARGUMENT_COUNT())
      INTEGER                              :: C
      CHARACTER*(COMMAND_ARGUMENT_COUNT()) :: D
      CHARACTER*(COMMAND_ARGUMENT_COUNT()) :: E(COMMAND_ARGUMENT_COUNT())
      COMPLEX                              :: F

      IF ( ALLOCATED(A) ) ERROR STOP 50
      ALLOCATE( A(COMMAND_ARGUMENT_COUNT()))
      IF (.not. ALLOCATED(A) .or. (SIZE(A) .ne. 5)) ERROR STOP 51
      DEALLOCATE(A)

      IF (SIZE(B) .ne. 5) ERROR STOP 52

      IF ( C .ne. 5 ) ERROR STOP 53

      if ( LEN(D) .ne. 5 ) &
      then
        error stop 54
      endif

      if ( (SIZE(E) .ne. 5) .or. (LEN(E(1)) .ne. 5)) &
      then
        error stop 55
      endif

      if ( F .ne. (5.0, 5.0) ) &
      then
        error stop 56
      endif


      END SUBROUTINE



