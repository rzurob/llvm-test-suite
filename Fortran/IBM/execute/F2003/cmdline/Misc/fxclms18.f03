! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec 1, 2003
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
!*  DESCRIPTION                : Call command line intrinsic routines with significant
!*                             : blanks as options and trailing blanks issued on
!*                             : command line( Environment variable with significant
!*                             : trailing blanks)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclms18


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      !The command line:
      !fxclms18 "  12  " "    " 22   !Ending with three trailing blanks
      !The environment variable:
      !CmdLine="fxclms18   12        22   "

      character(23)  :: CmdLine  = 'fxclms18   12        22'
      character(26)  :: EnvVar   = 'fxclms18   12        22   '
      character(6)   :: Option1 = '  12  '
      character(4)   :: Option2 = '    '
      character(2)   :: Option3 = '22'

      integer          :: CmdCount = 3
      integer          :: i, k
      character(2047)  :: Argument


      do k=1, 3

        if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
        then
          error stop 63
        endif

        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. CmdLine)  .or. &
             (LENGTH .ne. 23)              .or. &
             (STATUS .ne. 0) )                  &
        then
          error stop 64
        endif


        call GET_COMMAND_ARGUMENT(1, VALUE, LENGTH, STATUS)
        if ( (TRIM(VALUE) .ne. Option1)  .or. &
             (LENGTH      .ne. 6)        .or. &
             (STATUS      .ne. 0))            &
        then
          error stop 65
        endif

        call GET_COMMAND_ARGUMENT(2, VALUE, LENGTH, STATUS)
        if ( (TRIM(VALUE) .ne. Option2)  .or. &
             (LENGTH      .ne. 4)        .or. &
             (STATUS      .ne. 0))            &
        then
          error stop 66
        endif

        call GET_COMMAND_ARGUMENT(3, VALUE, LENGTH, STATUS)
        if ( (TRIM(VALUE) .ne. Option3)  .or. &
             (LENGTH      .ne. 2)        .or. &
             (STATUS      .ne. 0))            &
        then
          error stop 67
        endif

        call GET_COMMAND_ARGUMENT(0, VALUE, LENGTH, STATUS)
        if ( (TRIM(VALUE) .ne. 'fxclms18')  .or. &
             (LENGTH      .ne. 8)           .or. &
             (STATUS      .ne. 0))               &
        then
          error stop 68
        endif


        NAME = 'CmdLine    '
        TRIM_NAME = .true.

        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. EnvVar)  .or. &
             (LENGTH .ne. 26)           .or. &
             (STATUS .ne. 0))                &
        then
          error stop 69
        endif

      end do


      END



