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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing long strings as
!*                             : actual arguments.
!*                             : (to check if these intrinsic routine can handle those big size args)
!*                             : (Currently the maximum length for command line on all platform is
!*                             : less than 128k = 131072)
!234567890123456789012345678901234567890123456789012345678901234567890


      module  MOD

        character(131072)    :: COMMAND
        integer      	     :: LENGTH
        integer              :: STATUS
        integer              :: NUMBER
        character(131072)    :: VALUE
        integer              :: ARGCOUNT

        character(131072)    :: NAME
        logical              :: TRIM_NAME = .true.
        integer              :: CmdCount  = 5
        character(131072)    :: CmdLine

       !character(131072)    :: NAME      = 'CmdLine     '
       ! character(131072)   :: CmdLine   = 'fxclms02 ----- ++++++++++ 11111111111111 AAAAAAAA M-N'

      end module


      PROGRAM fxclms02

      USE MOD

      IMPLICIT NONE


      integer              :: i
      character(131072)    :: Argument


      NAME      = 'CmdLine     '
      CmdLine   = 'fxclms02 ----- ++++++++++ 11111111111111 AAAAAAAA M-N'

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


      DO NUMBER = 0, CmdCount

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      !Get options which do not exist
      call GET_COMMAND_ARGUMENT(CmdCount + 1, VALUE, LENGTH, STATUS)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 66
      endif

      !Get options which do not exist
      call GET_COMMAND_ARGUMENT( -1, VALUE, LENGTH, STATUS)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 67
      endif



      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)

      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 68
      endif

      ! Get AN ENVIRONMENT VARIABLE which does not exist. i.e. "_U_V_W__X_Y_Z_ "
      call GET_ENVIRONMENT_VARIABLE('_U_V_W__X_Y_Z_', VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 69
      endif


      END


      INCLUDE 'cmdline.include'

