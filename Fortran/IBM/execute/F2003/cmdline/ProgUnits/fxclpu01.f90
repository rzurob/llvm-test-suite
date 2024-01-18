! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu01 123 -ABC _ECF"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu01
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu01.f
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
!*  DESCRIPTION                : Call command line procedures through ext. subroutine  with arguments
!*                             : specified as components of derived type in modules and defined with
!*                             : private/public
!*                             : (test command line procedures arguments with public/private attribute)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module MOD

      PUBLIC

      type dertype
          PRIVATE
          SEQUENCE
          character(2049)   :: COMMAND
          integer           :: LENGTH
          integer           :: STATUS
          integer           :: NUMBER
          character(2047)   :: VALUE
          character(513)    :: NAME
          logical           :: TRIM_NAME
          integer           :: ARGCOUNT
      end type dertype


      character(2049)              :: CmdLine = 'fxclpu01 123 -ABC _ECF'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd

      CONTAINS

      SUBROUTINE MOD_SUB

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(cmd%COMMAND, cmd%LENGTH, cmd%STATUS)
      if ( (TRIM(cmd%COMMAND) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        cmd%NUMBER = i
        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE, cmd%LENGTH, cmd%STATUS)
        call MyGetArg(CmdLine, cmd%NUMBER, Argument)
        if ( (TRIM(cmd%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      cmd%NAME = 'CmdLine     '
      cmd%TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(cmd%NAME, cmd%VALUE, cmd%LENGTH, cmd%STATUS, cmd%TRIM_NAME)
      if ( (TRIM(cmd%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (cmd%STATUS .ne. 0))                       &
      then
        error stop 66
      endif



      END SUBROUTINE



      END MODULE


      PROGRAM fxclpu01
      USE MOD
      IMPLICIT NONE

      CALL EXT_SUB

      END

      SUBROUTINE EXT_SUB
      USE MOD

      CALL MOD_SUB

      END SUBROUTINE


      INCLUDE 'cmdline.include'




