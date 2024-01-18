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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing a long command line
!*                             : through a system call
!*                             : Imitate issuing a command directly on command line without involving
!*                             : a shell
!*                             : The maximum lengths (ARG_MAX) on various systems :
!*
!*                             : AIX:   /usr/include/sys/limits.h      24k = 24576
!*                             : MACOS: /usr/include/sys/syslimits.h   64k = 65536
!*                             : SLES:  /usr/include/linux/limits.h    128k= 131072
!*                             : RHEL:  /usr/include/linux/limits.h    128k= 131072
!*                             : This program is called from fxclms05.f
!234567890123456789012345678901234567890123456789012345678901234567890

    module  MOD

        character(500000)    :: COMMAND
        integer      	     :: LENGTH
        integer              :: STATUS
        integer              :: NUMBER
        character(500000)    :: VALUE
        integer              :: ARGCOUNT

        character(500000)    :: NAME
        logical              :: TRIM_NAME
        integer              :: CmdCount
        character(500000)    :: CmdLine


      end module


      PROGRAM fxclms050

      USE MOD

      IMPLICIT NONE

      integer              :: i,j, Num, l
      character(131072)    :: Argument

      CmdLine = 'fxclms050 ' // CmdLine    ! Form a command line
      CmdCount   = 2

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
      then
        error stop 63
      endif

      !Form the command line
      !No intrinsic can be used to convert string into character

      call GET_COMMAND_ARGUMENT(1, VALUE, LENGTH, STATUS)
      VALUE   = TRIM(VALUE)
      Num     = 0
      l       = LEN(TRIM(VALUE))

      DO i = 1, l
        Num = Num *10 + (ICHAR(VALUE(i:i)) - ICHAR('0'))
      END DO

      CmdLine = ' '
      DO i = 1, Num
        CmdLine(i:i) = 'x'
      END DO

      CmdLine = 'fxclms050 ' // TRIM(VALUE) // ' ' // trim(CmdLine)

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

      print*, 'Successfully executed fxclms050.f'

      END

      INCLUDE 'cmdline.include'



