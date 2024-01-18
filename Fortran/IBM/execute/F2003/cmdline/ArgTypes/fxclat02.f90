! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat02 abr,cd ef-gh -ijkl mnopq- -rst-   --uvw 1a2b xyz"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat02
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat02.f
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
!*  DESCRIPTION                : Pass components of a derived type  with allocatable attribute and
!*			       : defined in a module as actual arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        type dertype
          character(2049), allocatable :: COMMAND
          integer, allocatable         :: LENGTH
          character(4099)              :: STR     ! take spaces
          integer, allocatable         :: STATUS
          integer, allocatable         :: NUMBER
          character(2047), allocatable :: VALUE
          INTEGER                      :: ARR(10) ! take spaces
          character(513), allocatable  :: NAME
          logical, allocatable         :: TRIM_NAME
          integer, allocatable         :: ARGCOUNT
        end type dertype

      end module modtype


      PROGRAM fxclat02

      use modtype

      type(dertype) 	cmd
      character(4099) 	STR
      INTEGER         	ARR(10)
      INTEGER           i


      character(2049)              :: CmdLine = 'fxclat02 abr,cd ef-gh -ijkl mnopq- -rst- --uvw 1a2b xyz'
      integer                      :: CmdCount
      character(2047)              :: Argument

      if(allocated(cmd%COMMAND)   .or. allocated(cmd%LENGTH) .or. &
         allocated(cmd%STATUS)    .or. allocated(cmd%NUMBER) .or. &
         allocated(cmd%VALUE)     .or. allocated(cmd%NAME)   .or. &
         allocated(cmd%TRIM_NAME) .or. allocated(cmd%ARGCOUNT))   &
      then
        error stop 61
      endif

      allocate (cmd%COMMAND, cmd%LENGTH, cmd%STATUS, cmd%NUMBER, cmd%VALUE, cmd%NAME, cmd%TRIM_NAME, cmd%ARGCOUNT)

      if  (.not.allocated(cmd%COMMAND)    .or. .not.allocated(cmd%LENGTH) .or. &
           .not.allocated(cmd%STATUS)     .or. .not.allocated(cmd%NUMBER) .or. &
           .not.allocated(cmd%VALUE)      .or. .not.allocated(cmd%NAME)   .or. &
           .not.allocated(cmd%TRIM_NAME)  .or. .not.allocated(cmd%ARGCOUNT))   &
      then
        error stop 62
      endif

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 8 ) &
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

      DO i = 0, CmdCount

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


      deallocate (cmd%COMMAND, cmd%LENGTH, cmd%STATUS, cmd%NUMBER, cmd%VALUE, cmd%NAME, cmd%TRIM_NAME,cmd%ARGCOUNT)

      if(allocated(cmd%COMMAND)   .or. allocated(cmd%LENGTH) .or. &
         allocated(cmd%STATUS)    .or. allocated(cmd%NUMBER) .or. &
         allocated(cmd%VALUE)     .or. allocated(cmd%NAME)   .or. &
         allocated(cmd%TRIM_NAME) .or. allocated(cmd%ARGCOUNT))   &
      then
        error stop 68
      endif


      END

      INCLUDE 'cmdline.include'

