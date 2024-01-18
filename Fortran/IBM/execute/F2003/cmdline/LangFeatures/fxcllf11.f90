! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf11 ///aaa //bbbb//"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf11
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf10.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through nested loop
!*                             : control expression and loop body
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      MODULE MOD

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine


      DATA CmdLine    /'fxcllf11 ///aaa //bbbb//'/
      DATA NAME       /'CmdLine   '/
      DATA TRIM_NAME  /.true./


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      DATA COMMAND    / '????? '/
      DATA LENGTH     / 1111 /
      DATA STATUS     / 1111 /
      DATA NUMBER     /2222/
      DATA VALUE      / 100* '!'/
      DATA ARGCOUNT   / 0 /



      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i
      INTEGER    :: k, l, m, n


      END MODULE


      PROGRAM fxcllf11

      USE MOD

      IMPLICIT NONE



      DO k =1, COMMAND_ARGUMENT_COUNT()
      DO l =1, COMMAND_ARGUMENT_COUNT()
      DO m =1, COMMAND_ARGUMENT_COUNT()
      DO n =1, COMMAND_ARGUMENT_COUNT()


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
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif



      END DO
      END DO
      END DO
      END DO



      END



      INCLUDE 'cmdline.include'

