! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Invoke command line intrinsic routines through entries
!*                             : with different interfaces
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclpu11

      IMPLICIT NONE


      character(513)  :: NAME
      logical         :: TRIM_NAME
      character(2049) :: CmdLine

      INTEGER         :: i
      COMMON      CmdLine

      INTERFACE

        FUNCTION ENT_CMD_ARG_COUNT()
          LOGICAL                      :: ENT_CMD_ARG_COUNT
        END FUNCTION

        FUNCTION ENT_GET_COMMAND(  &
          NAME                   )
          character(513), INTENT(IN)   :: NAME
          LOGICAL                      :: ENT_GET_COMMAND
        END FUNCTION

        FUNCTION ENT_GET_CMD_ARG(  &
          NAME,                    &
          TRIM_NAME              )
          character(513), INTENT(IN)   :: NAME
          logical, INTENT(IN)          :: TRIM_NAME
          LOGICAL                      :: ENT_GET_CMD_ARG
        END FUNCTION

        FUNCTION ENT_GET_ENV_VAR(  &
          NAME,                    &
          TRIM_NAME              )
          character(513), INTENT(IN)   :: NAME
          logical, INTENT(IN)          :: TRIM_NAME
          LOGICAL                      :: ENT_GET_ENV_VAR
        END FUNCTION


      END INTERFACE

      CmdLine   = 'fxclpu11 +1 +2 +3 +4'
      NAME      = 'CmdLine   '
      TRIM_NAME = .true.



      IF (.not. ENT_CMD_ARG_COUNT()                   .or. &
          .not. ENT_GET_COMMAND(NAME)                 .or. &
          .not. ENT_GET_CMD_ARG(NAME, TRIM_NAME )     .or. &
          .not. ENT_GET_ENV_VAR(NAME, TRIM_NAME))          &
      ERROR STOP 61


      IF (.not.(ENT_CMD_ARG_COUNT()                   .and. &
                ENT_GET_COMMAND(NAME)                 .and. &
                ENT_GET_CMD_ARG(NAME, TRIM_NAME )     .and. &
                ENT_GET_ENV_VAR(NAME, TRIM_NAME)))          &
      ERROR STOP 62


      END



      FUNCTION EF_CMD   ( &
        NAME,             &
        TRIM_NAME       )

      character(513),  INTENT(IN)   :: NAME
      logical,         INTENT(IN)   :: TRIM_NAME

      LOGICAL EF_CMD
      LOGICAL ENT_CMD_ARG_COUNT
      LOGICAL ENT_GET_COMMAND
      LOGICAL ENT_GET_CMD_ARG
      LOGICAL ENT_GET_ENV_VAR

      character(2049), AUTOMATIC  :: COMMAND
      integer,         AUTOMATIC  :: LENGTH
      integer,         AUTOMATIC  :: STATUS
      integer,         AUTOMATIC  :: NUMBER
      character(2047), AUTOMATIC  :: VALUE
      integer,         AUTOMATIC  :: ARGCOUNT
      character(2047), AUTOMATIC  :: Argument


      integer, AUTOMATIC          :: CmdCount
      integer, AUTOMATIC          :: i

      character(2049)             :: CmdLine
      COMMON CmdLine

      INTERFACE

      PURE SUBROUTINE MyGetArg(CmdLine, Number, Arg)
        character*(*), intent(in)  :: CmdLine
        integer,       intent(out) :: Number
        character*(*), intent(in)  :: Arg
      END SUBROUTINE

      END INTERFACE


      EF_CMD = .true.
      RETURN


      ENTRY ENT_CMD_ARG_COUNT( )


      ENT_CMD_ARG_COUNT = .true.

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) &
      then
        ENT_CMD_ARG_COUNT = .false.
        !all zzrc(63)
      endif
      RETURN


      ENTRY ENT_GET_COMMAND(   &
        NAME               )

      ENT_GET_COMMAND = .true.
      call GET_COMMAND(COMMAND, LENGTH, STATUS)

      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        ENT_GET_COMMAND = .false.
        !error stop 64
      endif
      RETURN


      ENTRY ENT_GET_CMD_ARG(   &
        NAME,                  &
        TRIM_NAME          )

      CmdCount = COMMAND_ARGUMENT_COUNT()
      ENT_GET_CMD_ARG = .true.

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          ENT_GET_CMD_ARG = .false.
          !error stop 65
        endif

      END DO
      RETURN


      ENTRY ENT_GET_ENV_VAR(  &
        NAME,                 &
        TRIM_NAME           )

      ENT_GET_ENV_VAR = .true.

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
        ENT_GET_ENV_VAR = .false.
        !error stop 66
      endif
      RETURN



      END FUNCTION



      INCLUDE 'cmdline.include'


