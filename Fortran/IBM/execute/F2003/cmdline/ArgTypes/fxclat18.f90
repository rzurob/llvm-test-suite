! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat18 1 a 2 b 3"
! %COMPOPTS:  -qfree=f90 -qintsize=2
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat18
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat18.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing constants with
!*                             : various optional arguments as intent(in) actual arguments
!*                             : and set intsize=2
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        type dertype
          sequence
          character(2049)             :: COMMAND
          integer      	              :: LENGTH
          character(4099)             :: STR = '1234567890'
          integer                     :: STATUS
          integer                     :: NUMBER
          character(2047)             :: VALUE
          INTEGER                     :: ARR(10)
          integer                     :: ARGCOUNT
        end type dertype

      end module modtype


      PROGRAM fxclat18

      use modtype

      IMPLICIT NONE


      character(4099) 	STR
      INTEGER         	ARR(10)

      character(2049)              :: CmdLine = 'fxclat18 1 a 2 b 3'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd
      common /blk/cmd

      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 5 ) &
      then
        error stop 63_4
      endif

      call GET_COMMAND(cmd%COMMAND, cmd%LENGTH, cmd%STATUS)
      call GET_COMMAND()
      if ( (TRIM(cmd%COMMAND) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%STATUS .ne. 0) )                        &
      then
        error stop 64_4
      endif

      call GET_COMMAND(COMMAND, LENGTH)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))        &
      then
        error stop 65_4
      endif

      call GET_COMMAND(COMMAND)
      if ( TRIM(COMMAND) .ne. TRIM(CmdLine))  &
      then
        error stop 66_4
      endif


      DO i  = 0, CmdCount

        cmd%NUMBER = i
        call MyGetArg(CmdLine, cmd%NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE, cmd%LENGTH, cmd%STATUS)
        call GET_COMMAND_ARGUMENT(cmd%NUMBER)

        if ( (TRIM(cmd%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          error stop 67_4
        endif

        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE, cmd%LENGTH)
        if ( (TRIM(cmd%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument))) )     &
        then
          error stop 68_4
        endif

        call GET_COMMAND_ARGUMENT(cmd%NUMBER, VALUE =VALUE, STATUS=STATUS)
        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          error stop 69_4
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', cmd%VALUE, cmd%LENGTH, cmd%STATUS, .true.)
      if ( (TRIM(cmd%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (cmd%STATUS .ne. 0))                       &
      then
        error stop 70_4
      endif

      call GET_ENVIRONMENT_VARIABLE('CmdLine', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false.)
      if ( (cmd%LENGTH .ne.LENGTH)  .or. &
           (cmd%STATUS .ne. STATUS))     &
      then
        error stop 71_4
      endif


      END

      INCLUDE 'cmdline.include'






