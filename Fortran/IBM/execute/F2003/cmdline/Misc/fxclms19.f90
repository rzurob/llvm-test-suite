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
!*  DESCRIPTION                : Test command line intrinsic routines by passing a command
!*                             : through a script file which contains the command composed
!*                             : of multiple lines
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
      !fxclms19 \
      !012345678901234567890123456789012345678901234567890123456789 \
      !------------------------------------------------------------ \
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \
      !============================================================ \
      !____________________________________________________________


      character(313)  :: CmdLine  = 'fxclms19 &
      &012345678901234567890123456789012345678901234567890123456789 &
      &------------------------------------------------------------ &
      &++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ &
      &============================================================ &
      &____________________________________________________________'

      character(60)   :: Option(5)

      integer          :: CmdCount = 5
      integer          :: i, k
      character(2047)  :: Argument


      Option(1) = '012345678901234567890123456789012345678901234567890123456789'
      Option(2) = '------------------------------------------------------------'
      Option(3) = '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      Option(4) = '============================================================'
      Option(5) = '____________________________________________________________'


      do k=1, 3

        if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
        then
          error stop 63
        endif

        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. CmdLine)  .or. &
             (LENGTH .ne. 313 )            .or. &
             (STATUS .ne. 0) )                  &
        then
          error stop 64
        endif

        call GET_COMMAND_ARGUMENT(0, VALUE, LENGTH, STATUS)
        if ( (TRIM(VALUE) .ne. 'fxclms19')  .or. &
             (LENGTH      .ne. 8)           .or. &
             (STATUS      .ne. 0))               &
        then
          error stop 65
        endif

        do i = 1, 5
          call GET_COMMAND_ARGUMENT(i, VALUE, LENGTH, STATUS)
          if ( (TRIM(VALUE) .ne. Option(i))  .or. &
               (LENGTH      .ne. 60)       .or. &
               (STATUS      .ne. 0))            &
          then
            call zzrc(66+i)
          endif
        end do


      end do


      END



