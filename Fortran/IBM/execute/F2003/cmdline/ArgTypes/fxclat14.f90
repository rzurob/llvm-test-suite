! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat14 1 a"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat14
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat14.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing  components of
!*                             : allocatable derived type  initialized with specific values
!*                             : as actual arguments
!*

!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        type dertype
          character(2049)  :: COMMAND /'!!!!!!!!!!!!!!!!!!!!!!!!!11111'/
          integer      	   :: LENGTH       /2222/
          character(4099)  :: STR   /'???????????????????????????????'/  ! take spaces
          integer          :: STATUS    /3333/
          integer          :: NUMBER  /4444/
          character(2047)  :: VALUE   /'=========================================='/
          INTEGER          :: ARR(10) /10*100/ ! take spaces
          character(513)   :: NAME  /'CmdLine     '/
          logical          :: TRIM_NAME / .true./
          integer          :: ARGCOUNT  /5555/
        end type dertype

      end module modtype


      PROGRAM fxclat14

      use modtype

      IMPLICIT NONE

      character(2049)              :: CmdLine = 'fxclat14 1 a'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype), allocatable :: cmd

      allocate(cmd)

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 2 ) &
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

