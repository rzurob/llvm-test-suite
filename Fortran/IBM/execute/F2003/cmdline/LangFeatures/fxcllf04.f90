! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf04 \? \$ABC"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf04
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf04.f
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
!*  DESCRIPTION                : Call COMMAND_ARGUMENT_COUNT  in selector of  selectcase statement
!*                             :
!*
!234567890123456789012345678901234567890123456789012345678901234567890





      PROGRAM fxcllf04

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


      CmdLine   = 'fxcllf04 \\? $ABC'
      NAME      = 'CmdLine   '
      TRIM_NAME = .true.


      SELECTCASE(COMMAND_ARGUMENT_COUNT())
      CASE (2)


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 2 ) &
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
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif


      CASE DEFAULT
        error stop 67


      END SELECT


      END

      INCLUDE 'cmdline.include'






