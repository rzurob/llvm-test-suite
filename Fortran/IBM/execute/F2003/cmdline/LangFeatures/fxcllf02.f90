! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf02 1234_123 --123-- AAAAAAAAAAAAAAA"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf02
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf02.f
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
!*  DESCRIPTION                : Call command line intrinsic routines within main
!*                             : with arguments specified in common block and initialized in  block data
!*                             : Pack all the working variables together
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      BLOCK DATA

      TYPE MIXED
        sequence
        character(2049)  :: COMMAND
        character(2049)  :: CmdLine
        integer          :: LENGTH
        character(513)   :: NAME
        integer          :: STATUS
        character(2047)  :: VALUE
        logical          :: TRIM_NAME
        integer          :: NUMBER
        character(2047)  :: Argument
        integer          :: ARGCOUNT
        integer          :: CmdCount
        integer          :: i
      END TYPE

      TYPE(MIXED)        :: Args

      COMMON /COM/Args

      DATA Args%CmdLine/'fxcllf02 1234_123 --123-- AAAAAAAAAAAAAAA'/,  Args%NAME /'CmdLine   '/, Args%TRIM_NAME /.true./


      END BLOCK DATA




      PROGRAM fxcllf02

      IMPLICIT NONE


      TYPE MIXED
        sequence
        character(2049)  :: COMMAND
        character(2049)  :: CmdLine
        integer          :: LENGTH
        character(513)   :: NAME
        integer          :: STATUS
        character(2047)  :: VALUE
        logical          :: TRIM_NAME
        integer          :: NUMBER
        character(2047)  :: Argument
        integer          :: ARGCOUNT
        integer          :: CmdCount
        integer          :: i ! iusing as loop index is not allowed
      END TYPE

      TYPE(MIXED)        :: Args
      INTEGER            :: i

      COMMON /COM/Args

      Args%CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( Args%CmdCount .ne. 3 ) &
      then
        error stop 63
      endif


      call GET_COMMAND(Args%COMMAND, Args%LENGTH, Args%STATUS)
      if ( (TRIM(Args%COMMAND) .ne. TRIM(Args%CmdLine))  .or. &
           (Args%LENGTH .ne. LEN(TRIM(Args%CmdLine)))    .or. &
           (Args%STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


      DO i  = 0, Args%CmdCount

        Args%NUMBER = i
        call GET_COMMAND_ARGUMENT(Args%NUMBER, Args%VALUE, Args%LENGTH, Args%STATUS)
        call MyGetArg(Args%CmdLine, Args%NUMBER, Args%Argument)

        if ( (TRIM(Args%VALUE) .ne. TRIM(Args%Argument))       .or. &
             (Args%LENGTH      .ne. LEN(TRIM(Args%Argument)))  .or. &
             (Args%STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO



      call GET_ENVIRONMENT_VARIABLE(Args%NAME, Args%VALUE, Args%LENGTH, Args%STATUS, Args%TRIM_NAME)
      if ( (TRIM(Args%VALUE) .ne. TRIM(Args%CmdLine))  .or. &
            (Args%LENGTH .ne. LEN(TRIM(Args%CmdLine)))  .or. &
            (Args%STATUS .ne. 0))                       &
      then
         error stop 66
      endif


      END

      INCLUDE 'cmdline.include'



