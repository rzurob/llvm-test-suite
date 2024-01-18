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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing variables defined
!*                             : in a module with the same name as the augument keywords
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

          character(2049)             :: COMMAND
          integer      	              :: LENGTH
          character(4099)             :: STR
          integer                     :: STATUS
          integer                     :: NUMBER
          character(2047)             :: VALUE
          character(513)              :: NAME
          logical                     :: TRIM_NAME
          INTEGER                     :: ARR(10)
          integer                     :: ARGCOUNT


      end module modtype


      PROGRAM fxclat24

      use modtype, COMMAND  => COMMAND
      use modtype, LENGTH   => LENGTH
      use modtype, STATUS   => STATUS
      use modtype, NUMBER   => NUMBER
      use modtype, VALUE    => VALUE
      use modtype, NAME     => NAME
      use modtype, TRIM_NAME     => TRIM_NAME
      use modtype, ARGCOUNT => ARGCOUNT



      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat24 -sdfgsd sdfgd-sfgdsfg dsfgdsfg +_23453245'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND=COMMAND(33:532), LENGTH=LENGTH, STATUS=STATUS)
      call GET_COMMAND()
      if ( (TRIM(COMMAND(33:532)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (STATUS .ne. 0) )                                &
      then
        error stop 64
      endif

      call GET_COMMAND(COMMAND=COMMAND(1:101), LENGTH=LENGTH)
      if ( (TRIM(COMMAND(1:101)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))               &
      then
        error stop 65
      endif

      call GET_COMMAND(COMMAND=COMMAND(1001:2049))
      if ( TRIM(COMMAND(1001:2049)) .ne. TRIM(CmdLine))  &
      then
        error stop 66
      endif


      DO i  = 0, CmdCount

        NUMBER = i
        call MyGetArg(CmdLine, NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(NUMBER=NUMBER, VALUE=VALUE(1023:2046), LENGTH=LENGTH,STATUS=STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER=NUMBER)

        if ( (TRIM(VALUE(1023:2046)) .ne. TRIM(Argument))     .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))           .or. &
             (STATUS      .ne. 0) )                                &
        then
          error stop 67
        endif

        call GET_COMMAND_ARGUMENT(NUMBER=NUMBER, VALUE=VALUE(513:688), LENGTH=LENGTH)
        if ( (TRIM(VALUE(513:688)) .ne. TRIM(Argument))      .or. &
             (LENGTH               .ne. LEN(TRIM(Argument))))     &
        then
          error stop 68
        endif

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE =VALUE(11:511), STATUS=STATUS)
        if ( (TRIM(VALUE(11:511)) .ne. TRIM(Argument))  .or. &
             (STATUS      .ne. 0) )                          &
        then
          error stop 69
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', VALUE=VALUE(1013:2039), LENGTH=LENGTH, STATUS=STATUS, TRIM_NAME=.true.)
      if ( (TRIM(VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (STATUS .ne. 0))                                  &
      then
        error stop 70
      endif

      call GET_ENVIRONMENT_VARIABLE('CmdLine', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false.)
      if ( (LENGTH .ne.LENGTH)  .or. &
           (STATUS.ne. STATUS))     &
      then
        error stop 71
      endif


      END


      INCLUDE 'cmdline.include'






