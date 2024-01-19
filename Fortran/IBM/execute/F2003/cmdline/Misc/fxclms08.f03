! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov 1, 2003
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
!*  DESCRIPTION                : Tests GET_ENVIRONMENT_VARIABLE by exporting a series
!*                             : of similar environment variables and reading them in
!*                             : Note: "_" is a system reserved variable
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclms08

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclms08 ____'
      integer                      :: CmdCount = 1
      integer                      :: i, k
      character(2047)              :: Argument, EnvVar


      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT


!     call GET_ENVIRONMENT_VARIABLE('_', VALUE, LENGTH, STATUS, .true.)

!     if ( (TRIM(VALUE) .ne. 'fxclms08')  .or. &
!          (LENGTH .ne. 8)                .or. &
!          (STATUS .ne. 0))                    &
!     then
!       error stop 70
!     endif
!     Cancel this test as aix returns "fxclms08", mac returns "./fxclms08"

      EnvVar = '_'

      do k=2, 5

        EnvVar = '_' // EnvVar
        call GET_ENVIRONMENT_VARIABLE(EnvVar, VALUE, LENGTH, STATUS, .true.)

        if ( (TRIM(VALUE) .ne. '_')  .or. &
             (LENGTH .ne. 1)         .or. &
             (STATUS .ne. 0))             &
        then
          error stop 71
        endif


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


        call GET_ENVIRONMENT_VARIABLE( 'CmdLine', VALUE, LENGTH, STATUS, .false.)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif


      end do


      END

      INCLUDE 'cmdline.include'


