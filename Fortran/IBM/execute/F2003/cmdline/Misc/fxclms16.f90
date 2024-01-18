! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms16 -1 -a -2 -B -;fxclms16 -1 -a -2 -B -;fxclms16 -1 -a -2 -B -;fxclms16 -1 -a -2 -B -"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms16
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms16.f
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
!*  DESCRIPTION                : Test command line intrinsic routines with ";" issued
!*                             : on command line
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclms16


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)  :: CmdLine  = 'fxclms16 -1 -a -2 -B - '
      character(2049)  :: CmdLine1 = "fxclms16 -1 -a -2 -B -;fxclms16 -1 -a -2 -B -;fxclms16 -1 -a -2 -B -;fxclms16 -1 -a -2 -B -"
      integer          :: CmdCount = 5
      integer          :: i, k
      character(2047)  :: Argument


      do k=1, 10

        if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
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

        NAME = 'CmdLine    '
        TRIM_NAME = .true.



        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine1))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine1)))  .or. &
             (STATUS .ne. 0))                        &
        then
          error stop 66
        endif

      end do


      END

      INCLUDE 'cmdline.include'


