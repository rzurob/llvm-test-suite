! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing  return values from
!*                             : intrinsic functions (RSHIFT /LSHIFT/TRIM)
!*                             : on  typeless constants as intent(in) arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclat27

      IMPLICIT NONE


      character(2049)          :: CmdLine = 'fxclat27 00000 ========= aaaaaaaaaaa'
      integer                  :: CmdCount, i
      character(2047)          :: Argument


      character(2049), allocatable             :: COMMAND
      integer, allocatable                     :: LENGTH
      integer, allocatable                     :: STATUS
      integer, allocatable                     :: NUMBER
      character(2047), allocatable             :: VALUE
      integer, allocatable                     :: ARGCOUNT

      allocate(COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT)

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND(2:2048), LENGTH, STATUS)
      call GET_COMMAND()

      if ( (TRIM(COMMAND(2:2048)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (STATUS .ne. 0) )                                &
      then
        error stop 64
      endif

      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:2046))
      if ( (TRIM(COMMAND(1:2046)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))                &
      then
        error stop 65
      endif

      call GET_COMMAND(COMMAND=COMMAND(1:2049))
      if ( TRIM(COMMAND(1:2049)) .ne. TRIM(CmdLine))  &
      then
        error stop 66
      endif


      call MyGetArg(CmdLine, 3, Argument)

      call GET_COMMAND_ARGUMENT(RSHIFT(b'000000110', 1), VALUE(1023:2046), LENGTH, STATUS)
      call GET_COMMAND_ARGUMENT(NUMBER)
      if ( (TRIM(VALUE(1023:2046)) .ne. TRIM(Argument))   .or. &
           (LENGTH      .ne. LEN(TRIM(Argument)))         .or. &
           (STATUS      .ne. 0) )                              &
      then
        error stop 67
      endif

      call MyGetArg(CmdLine,2, Argument)

      call GET_COMMAND_ARGUMENT(LSHIFT(o'0001',1), VALUE(513:688), LENGTH)
      if ( (TRIM(VALUE(513:688)) .ne. TRIM(Argument)) .or. &
           (LENGTH      .ne. LEN(TRIM(Argument))))         &
      then
        error stop 68
      endif

      call MyGetArg(CmdLine, 1, Argument)
      call GET_COMMAND_ARGUMENT(RSHIFT(z'00010', 4) , VALUE =VALUE(11:511), STATUS=STATUS)
      if ( (TRIM(VALUE(11:511)) .ne. TRIM(Argument))       .or. &
          (STATUS      .ne. 0) )                                &
      then
        error stop 69
      endif



      ! z'436D644C696E652020'  == 'CmdLine  '
      call GET_ENVIRONMENT_VARIABLE(TRIM(z'436D644C696E652020'), VALUE(1013:2039), LENGTH, STATUS, .true. .or. .true.)
      if ( (TRIM(VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (STATUS .ne. 0))                                  &
      then
        error stop 70
      endif

      !o'206665442306455631220040' == 'CmdLine  '
      call GET_ENVIRONMENT_VARIABLE(TRIM(o'206665442306455631220040'), LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false. .and. .true.)
      if ( (LENGTH .ne.LENGTH)  .or. &
           (STATUS .ne. STATUS))     &
      then
        error stop 71
      endif

      deallocate(COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT)


      END

      INCLUDE 'cmdline.include'



