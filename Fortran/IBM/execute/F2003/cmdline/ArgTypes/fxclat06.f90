! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing pointer components of
!*                             : derived type as actual arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        type dertype
          sequence
          character(2049), POINTER  :: COMMAND
          integer ,        POINTER  :: LENGTH
          integer,         POINTER  :: STATUS
          integer ,        POINTER  :: NUMBER
          character(2047), POINTER  :: VALUE
          character(513),  POINTER  :: NAME
          logical,         POINTER  :: TRIM_NAME
          integer ,        POINTER  :: ARGCOUNT
        end type dertype

      end module modtype


      PROGRAM fxclat06

      use modtype

      IMPLICIT NONE
      character(2049), TARGET  :: COMMAND
      integer,         TARGET  :: LENGTH
      character(4099), TARGET  :: STR
      integer,         TARGET  :: STATUS
      integer,         TARGET  :: NUMBER
      character(2047), TARGET  :: VALUE
      INTEGER,         TARGET  :: ARR(10)
      character(513),  TARGET  :: NAME
      logical,         TARGET  :: TRIM_NAME
      integer,         TARGET  :: ARGCOUNT


      character(2049)              :: CmdLine = 'fxclat06 1 a 2 b 3'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd
      common /blk/COMMAND, LENGTH, STATUS, NUMBER, VALUE, NAME, TRIM_NAME, ARGCOUNT
      common cmd

      cmd%COMMAND    => COMMAND
      cmd%LENGTH     => LENGTH
      cmd%STATUS     => STATUS
      cmd%NUMBER     => NUMBER
      cmd%VALUE      => VALUE
      cmd%NAME       => NAME
      cmd%TRIM_NAME  => TRIM_NAME
      cmd%ARGCOUNT   => ARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 5 ) &
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

      call GET_ENVIRONMENT_VARIABLE(cmd%NAME, cmd%VALUE, cmd%LENGTH, cmd%STATUS, cmd%TRIM_NAME)
      if ( (TRIM(cmd%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (cmd%STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      END

      INCLUDE 'cmdline.include'


      BLOCK DATA BLOCKDATA

        character(2049)   :: COMMAND
        integer           :: LENGTH
        character(4099)   :: STR
        integer           :: STATUS
        integer           :: NUMBER
        character(2047)   :: VALUE
        INTEGER           :: ARR(10)
        character(513)    :: NAME
        logical           :: TRIM_NAME
        integer           :: ARGCOUNT

        common /blk/COMMAND, LENGTH, STATUS, NUMBER, VALUE, NAME, TRIM_NAME, ARGCOUNT

        DATA NAME /'CmdLine     '/
        DATA TRIM_NAME  / .true./

      END BLOCK DATA



