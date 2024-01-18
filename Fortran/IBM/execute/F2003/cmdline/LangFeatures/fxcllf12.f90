! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Call command line intrinsic routines through nested do while
!*                             : control expression and loop body
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      MODULE MOD

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine


      DATA CmdLine    /'fxcllf12 ---rrr -/-/-/-'/
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


      PROGRAM fxcllf12

      USE MOD

      IMPLICIT NONE


      k = 1
      DO WHILE ( k .le. COMMAND_ARGUMENT_COUNT() )
      l = 1
      DO WHILE ( l .le. COMMAND_ARGUMENT_COUNT() )
      m = 1
      DO WHILE ( m .le. COMMAND_ARGUMENT_COUNT() )
      n = 1
      DO WHILE ( n .le. COMMAND_ARGUMENT_COUNT() )


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


      n = n + 1
      END DO
      m = m + 1
      END DO
      l = l + 1
      END DO
      k = k +1
      END DO



      END



      INCLUDE 'cmdline.include'

