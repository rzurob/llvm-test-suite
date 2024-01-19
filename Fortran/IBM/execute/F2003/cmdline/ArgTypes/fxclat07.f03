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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing pointer
!*                             : components (pointing to allocatable objects) of derived type
!*                             : as arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module modtype

        type dertype
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


      PROGRAM fxclat07

      use modtype

      IMPLICIT NONE

      character(2049), ALLOCATABLE, TARGET  :: COMMAND
      integer,         ALLOCATABLE, TARGET  :: LENGTH
      character(4099), ALLOCATABLE, TARGET  :: STR
      integer,         ALLOCATABLE, TARGET  :: STATUS
      integer,         ALLOCATABLE, TARGET  :: NUMBER
      character(2047), ALLOCATABLE, TARGET  :: VALUE
      INTEGER,         ALLOCATABLE, TARGET  :: ARR(:)
      character(513),  ALLOCATABLE, TARGET  :: NAME
      logical,         ALLOCATABLE, TARGET  :: TRIM_NAME
      integer,         ALLOCATABLE, TARGET  :: ARGCOUNT


      character(2049)              :: CmdLine = 'fxclat07'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd

      if(allocated(COMMAND)   .or. allocated(LENGTH) .or. &
         allocated(STATUS)    .or. allocated(NUMBER) .or. &
         allocated(VALUE)     .or. allocated(NAME)   .or. &
         allocated(TRIM_NAME) .or. allocated(ARGCOUNT))   &
      then
        error stop 61
      endif

      allocate (COMMAND, LENGTH, STATUS, NUMBER, VALUE, NAME, TRIM_NAME, ARGCOUNT)

      if  (.not.allocated(COMMAND)    .or. .not.allocated(LENGTH) .or. &
           .not.allocated(STATUS)     .or. .not.allocated(NUMBER) .or. &
           .not.allocated(VALUE)      .or. .not.allocated(NAME)   .or. &
           .not.allocated(TRIM_NAME)  .or. .not.allocated(ARGCOUNT))   &
      then
        error stop 62
      endif

      cmd%COMMAND    => COMMAND
      cmd%LENGTH     => LENGTH
      cmd%STATUS     => STATUS
      cmd%NUMBER     => NUMBER
      cmd%VALUE      => VALUE
      cmd%NAME       => NAME
      cmd%TRIM_NAME  => TRIM_NAME
      cmd%ARGCOUNT   => ARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 0 ) &
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


      deallocate (COMMAND, LENGTH, STATUS, NUMBER, VALUE, NAME, TRIM_NAME, ARGCOUNT)

      if(allocated(COMMAND)   .or. allocated(LENGTH) .or. &
         allocated(STATUS)    .or. allocated(NUMBER) .or. &
         allocated(VALUE)     .or. allocated(NAME)   .or. &
         allocated(TRIM_NAME) .or. allocated(ARGCOUNT))   &
      then
        error stop 68
      endif

      END

      INCLUDE 'cmdline.include'






