! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms06 0123456789"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms06
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms06.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing Arithmetic/ logical
!*                             : expresssions with various optional arguments as intent(in) arguments.
!*                             : Intent(out) arguments are passed with limited length.
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclms06

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclms06 0123456789'
      integer                      :: CmdCount = 1
      integer                      :: i
      character(2047)              :: Argument


      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT



      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
      then
        error stop 63
      endif

      call GET_COMMAND(STATUS=STATUS, COMMAND=COMMAND(33:532), LENGTH=LENGTH)
      call GET_COMMAND(STATUS=STATUS, LENGTH=LENGTH)   ! no effect on COMMAND(33:532)

      if ( (TRIM(COMMAND(33:532)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (STATUS .ne. 0) )                                &
      then
        error stop 64
      endif
      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:19))  ! Just enough to hold the content
  !   call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:20))  ! Just enough to hold the content
  !   call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:18))  ! Just enough to hold the content
      if ( (TRIM(COMMAND(1:19)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))              &
      then
        error stop 65
      endif

      call GET_COMMAND(COMMAND=COMMAND(1001:1019))
      if ( TRIM(COMMAND(1001:1019)) .ne. TRIM(CmdLine))  &
      then
        error stop 66
      endif

      DO i  = 0, CmdCount

        NUMBER = i
        call MyGetArg(CmdLine, NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(MOD(NUMBER, NUMBER + 1), VALUE(1001:1019), LENGTH, STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER)  ! No effect on thers

        if ( (TRIM(VALUE(1001:1019)) .ne. TRIM(Argument))   .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))         .or. &
             (STATUS      .ne. 0) )                              &
        then
          error stop 67
        endif

        call GET_COMMAND_ARGUMENT(2*i + i +NUMBER-3*i, VALUE(513:688), LENGTH)
        if ( (TRIM(VALUE(513:688)) .ne. TRIM(Argument))      .or. &
             (LENGTH      .ne. LEN(TRIM(Argument))))              &
        then
          error stop 68
        endif

        call GET_COMMAND_ARGUMENT(VALUE =VALUE(11:511), STATUS=STATUS, NUMBER=NUMBER + mod(5, 5))
        if ( (TRIM(VALUE(11:511)) .ne. TRIM(Argument))       .or. &
             (STATUS      .ne. 0) )                               &
        then
          error stop 69
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE('CmdLine   '// '     ' // '       ' // '      ', VALUE(1013:2039), LENGTH, STATUS, .true. .or. .true.)
      if ( (TRIM(VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (STATUS .ne. 0))                                 &
      then
        error stop 70
      endif

      call GET_ENVIRONMENT_VARIABLE(LENGTH=LENGTH, STATUS= STATUS, &
           TRIM_NAME=.true. .and. .true., NAME='CmdLine ' // ' ' // ' ' )
      if ( (LENGTH .ne.LENGTH)  .or. &
           (STATUS .ne. STATUS))     &
      then
        error stop 71
      endif


      END

      INCLUDE 'cmdline.include'





