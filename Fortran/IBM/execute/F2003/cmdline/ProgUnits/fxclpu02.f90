! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu02 ::: ... ,,, \!"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu02
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu02.f
!*
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Call command line procedures through a call chain
!*                             : with the same name used as external subroutine
!234567890123456789012345678901234567890123456789012345678901234567890

      module MOD

      character(2049)   :: COMMAND
      integer           :: LENGTH
      integer           :: STATUS
      integer           :: NUMBER
      character(2047)   :: VALUE
      character(513)    :: NAME
      logical           :: TRIM_NAME
      integer           :: ARGCOUNT


      character(2049)              :: CmdLine = 'fxclpu02 ::: ... ,,, \\!'
      integer                      :: CmdCount, i
      character(2047)              :: Argument



      END MODULE


      PROGRAM fxclpu02
      IMPLICIT NONE


      CALL COMMAND_ARGUMENT_COUNT

      END


      SUBROUTINE COMMAND_ARGUMENT_COUNT
      USE MOD

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      CALL GET_COMMAND

      END SUBROUTINE


      SUBROUTINE  GET_COMMAND
      USE MOD

      EXTERNAL GET_ENVIRONMENT_VARIABLE

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) &
      then
        error stop 63
      endif

      CALL GET_ENVIRONMENT_VARIABLE

      END SUBROUTINE


      SUBROUTINE  GET_ENVIRONMENT_VARIABLE
      USE MOD


      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)
        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      CALL FINAL

      END SUBROUTINE


      SUBROUTINE FINAL
      USE MOD

      NAME = 'CmdLine     '
      TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE


      INCLUDE 'cmdline.include'










