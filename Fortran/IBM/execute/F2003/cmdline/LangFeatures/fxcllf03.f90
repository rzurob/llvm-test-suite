! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf03 +++ ++AAA VVV____ S 5"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf03
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf03.f
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
!*  DESCRIPTION                : Call COMMAND_ARGUMENT_COUNT  in  Computed GOTO statement
!*                             :
!*
!234567890123456789012345678901234567890123456789012345678901234567890





      PROGRAM fxcllf03

      IMPLICIT NONE

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


      CmdLine   = 'fxcllf03 +++ ++AAA VVV____ S 5'
      NAME      = 'CmdLine   '
      TRIM_NAME = .true.

      GOTO (40,20,30,10, 1), COMMAND_ARGUMENT_COUNT()
10    error stop 67

20    error stop 67

30    error stop 67

40    error stop 67


1     CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 5 ) &
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



      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif


      END

      INCLUDE 'cmdline.include'




