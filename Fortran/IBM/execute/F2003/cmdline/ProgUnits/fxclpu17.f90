! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu17 //\? //\? ///"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu17
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpuf17.f
!*
!*  DATE                       : Oct 1, 2003
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
!*  DESCRIPTION                : Declare variables mixed with other data within
!*                             : blank common block
!*                             : Invoke procedures inside int. func and int. subs
!*                             :   (blank common block and host association)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclpu17
      IMPLICIT NONE


      character(513)   :: NAME
      character(513)   :: NAME1
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine
      character(2049)  :: CmdLine1
      integer          :: CmdCount

      COMMON /sargs/CmdLine,CmdLine1, NAME, NAME1, TRIM_NAME, CmdCount

      character(2049)  :: COMMAND
      character(2049)  :: COMMAND1
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE1
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      COMMON /cargs/COMMAND, COMMAND1, LENGTH, STATUS, NUMBER, VALUE1, VALUE, ARGCOUNT

      INTEGER Junk


      CmdLine   = 'fxclpu17 //\\? //\\? ///'
      CmdLine1  = '????????????????????????'
      NAME      = 'CmdLine   '
      NAME1     = '??????????'
      TRIM_NAME = .true.
      CmdCount  = 3


      COMMAND  = '???????????????????'
      COMMAND1 = '                   '
      LENGTH   = 9999
      STATUS   = 9999
      NUMBER   = 9999
      VALUE1   = '              '
      VALUE    = '???????????????'
      ARGCOUNT = 9999


      Junk = INTF_CMD_ARG_COUNT()

      CALL INTS_GET_CMD


      CALL INTS_GET_CMD_ARG


      CALL INTS_GET_ENV_VAR


      CONTAINS


      FUNCTION INTF_CMD_ARG_COUNT()

      INTEGER INTF_CMD_ARG_COUNT


      NAME1    = '22222222222222222222222222222'
      CmdLine1 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      COMMAND1 = 'yyyyyyyyyyyyyyyyyyyyyy'
      VALUE1   = 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      INTF_CMD_ARG_COUNT = CmdCount

      END FUNCTION


      SUBROUTINE INTS_GET_CMD()


      NAME1    = '22222222222222222222222222222'
      CmdLine1 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      COMMAND1 = 'yyyyyyyyyyyyyyyyyyyyyy'
      VALUE1   = 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
         error stop 64
      endif

      END SUBROUTINE



      SUBROUTINE INTS_GET_CMD_ARG()

      INTEGER  i
      CHARACTER(2049) Argument

      NAME1    = '22222222222222222222222222222'
      CmdLine1 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      COMMAND1 = 'yyyyyyyyyyyyyyyyyyyyyy'
      VALUE1   = 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'


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

      END SUBROUTINE



      SUBROUTINE INTS_GET_ENV_VAR()


      NAME1    = '22222222222222222222222222222'
      CmdLine1 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      COMMAND1 = 'yyyyyyyyyyyyyyyyyyyyyy'
      VALUE1   = 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'


      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
         error stop 66
      endif

      END SUBROUTINE


      END



      INCLUDE 'cmdline.include'

