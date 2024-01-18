! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat42 @@@@@@@@@@@@@@@@@@@@ -@@@@@- +"
! %COMPOPTS:  -qfree=f90  -qintsize=8
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat42
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat42.f
!*
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing a variety lengths of
!*                             : typeless constants and pointers with intsize = 8 as actual arguments
!*                             :
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclat42

      IMPLICIT NONE

      character(2049), pointer :: COMMAND
      integer,         pointer :: LENGTH
      integer,         pointer :: STATUS
      integer,         pointer :: NUMBER
      character(2049), pointer :: VALUE
      character(2049), pointer :: NAME
      logical,         pointer :: TRIM_NAME
      integer,         pointer :: ARGCOUNT

      character(2049), target :: TCOMMAND
      integer,         target :: TLENGTH
      integer,         target :: TSTATUS
      integer,         target :: TNUMBER
      character(2049), target :: TVALUE
      character(2049), target :: TNAME
      logical,         target :: TTRIM_NAME
      integer,         target :: TARGCOUNT

      character(2049)        ::  CmdLine = 'fxclat42 @@@@@@@@@@@@@@@@@@@@ -@@@@@- +'
      integer                ::  CmdCount, i
      character(2047)        ::  Argument



      COMMAND   => TCOMMAND
      LENGTH    => TLENGTH
      STATUS    => TSTATUS
      STATUS    => TSTATUS
      VALUE     => TVALUE
      NAME      => TNAME
      TRIM_NAME => TTRIM_NAME
      ARGCOUNT  => TARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63_4
      endif

      call GET_COMMAND(COMMAND(2:2048), LENGTH, STATUS)
      call GET_COMMAND()

      if ( (TRIM(COMMAND(2:2048)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (STATUS .ne. 0) )                                &
      then
        error stop 64_4
      endif

      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:2046))
      if ( (TRIM(COMMAND(1:2046)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))                &
      then
        error stop 65_4
      endif

      call GET_COMMAND(COMMAND=COMMAND(1:2049))
      if ( TRIM(COMMAND(1:2049)) .ne. TRIM(CmdLine))  &
      then
        error stop 66_4
      endif


      call MyGetArg(CmdLine, 3, Argument)

      call GET_COMMAND_ARGUMENT(b'00000011', VALUE(1023:2046), LENGTH, STATUS)
      call GET_COMMAND_ARGUMENT( 0 )
      if ( (TRIM(VALUE(1023:2046)) .ne. TRIM(Argument))   .or. &
           (LENGTH      .ne. LEN(TRIM(Argument)))         .or. &
           (STATUS      .ne. 0) )                              &
      then
        error stop 67_4
      endif

      call MyGetArg(CmdLine,2, Argument)
      call GET_COMMAND_ARGUMENT(o'0002', VALUE(513:688), LENGTH)
      if ( (TRIM(VALUE(513:688)) .ne. TRIM(Argument)) .or. &
           (LENGTH      .ne. LEN(TRIM(Argument))))         &
      then
        error stop 68_4
      endif

      call MyGetArg(CmdLine, 1, Argument)
      call GET_COMMAND_ARGUMENT(z'0001', VALUE =VALUE(11:511), STATUS=STATUS)
      if ( (TRIM(VALUE(11:511)) .ne. TRIM(Argument))       .or. &
          (STATUS      .ne. 0) )                                &
      then
        error stop 69_4
      endif



      ! z'436D644C696E652020'  == 'CmdLine  '
      call GET_ENVIRONMENT_VARIABLE(z'436D644C696E652020', VALUE(1013:2039), LENGTH, STATUS, .true. .or. .true.)
      if ( (TRIM(VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (STATUS .ne. 0))                                  &
      then
        error stop 70_4
      endif

      !o'206665442306455631220040' == 'CmdLine  '
      call GET_ENVIRONMENT_VARIABLE(o'206665442306455631220040', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false. .and. .true.)
      if ( (LENGTH .ne.LENGTH)  .or. &
           (STATUS .ne. STATUS))     &
      then
        error stop 71_4
      endif


      END

      INCLUDE 'cmdline.include'




