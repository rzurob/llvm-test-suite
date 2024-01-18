! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu04 1 a 2"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu04
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu04.f
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
!*  DESCRIPTION                : Call command line procedures through ext. subroutine
!*                             : whose dummy arguments are used as command line routines'
!*                             : actual arguments
!*                             : the ">" part becomes one of options
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(2049)   :: COMMAND
      integer           :: LENGTH
      integer           :: STATUS
      integer           :: NUMBER
      character(2047)   :: VALUE
      character(513)    :: NAME
      logical           :: TRIM_NAME
      integer           :: ARGCOUNT

      END MODULE


      PROGRAM fxclpu04
      USE MOD
      IMPLICIT NONE


      CALL EXT_SUB(            &
                    COMMAND,   &
                    LENGTH,    &
                    STATUS,    &
                    NUMBER,    &
                    VALUE  ,   &
                    NAME  ,    &
                    TRIM_NAME ,&
                    ARGCOUNT   )



      END

      SUBROUTINE EXT_SUB(  &
                          COMMAND,   &
                          LENGTH,    &
                          STATUS,    &
                          NUMBER,    &
                          VALUE,     &
                          NAME,      &
                          TRIM_NAME, &
                          ARGCOUNT   )

      character(2049)   :: COMMAND
      integer           :: LENGTH
      integer           :: STATUS
      integer           :: NUMBER
      character(2047)   :: VALUE
      character(513)    :: NAME
      logical           :: TRIM_NAME
      integer           :: ARGCOUNT

      character(2049)              :: CmdLine = 'fxclpu04 1 a 2'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


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










