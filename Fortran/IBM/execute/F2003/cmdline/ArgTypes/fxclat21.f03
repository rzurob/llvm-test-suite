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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing arithmatic/character
!*                             : expresssions with various optional arguments as intent(in) arguments
!*                             : Set default int sizt=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclat21

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat21 1 a 2 b 3 c'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT



      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 6 ) &
      then
        call zzrcy4(63)
      endif

      call GET_COMMAND(COMMAND(33:532), LENGTH, STATUS)
      call GET_COMMAND()

      if ( (TRIM(COMMAND(33:532)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (STATUS .ne. 0) )                                &
      then
        call zzrcy4(64)
      endif

      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:101))
      if ( (TRIM(COMMAND(1:101)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))               &
      then
        call zzrcy4(65)
      endif

      call GET_COMMAND(COMMAND=COMMAND(1001:2049))
      if ( TRIM(COMMAND(1001:2049)) .ne. TRIM(CmdLine))  &
      then
        call zzrcy4(66)
      endif

      DO i  = 0, CmdCount

        NUMBER = i
        call MyGetArg(CmdLine, NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(i+NUMBER-i, VALUE(1023:2046), LENGTH, STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER)

        if ( (TRIM(VALUE(1023:2046)) .ne. TRIM(Argument))   .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))         .or. &
             (STATUS      .ne. 0) )                              &
        then
          call zzrcy4(67)
        endif

        call GET_COMMAND_ARGUMENT(2*i + i +NUMBER-3*i, VALUE(513:688), LENGTH)
        if ( (TRIM(VALUE(513:688)) .ne. TRIM(Argument))   .or. &
             (LENGTH      .ne. LEN(TRIM(Argument))))           &
        then
          call zzrcy4(68)
        endif

        call GET_COMMAND_ARGUMENT(NUMBER + mod(5, 5), VALUE =VALUE(11:511), STATUS=STATUS)
        if ( (TRIM(VALUE(11:511)) .ne. TRIM(Argument))       .or. &
             (STATUS      .ne. 0) )                               &
        then
          call zzrcy4(69)
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE('CmdLine   '// '     ' // '       ' // '      ', VALUE(1013:2039), LENGTH, STATUS, .true. .or. .true.)
      if ( (TRIM(VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (STATUS .ne. 0))                                  &
      then
        call zzrcy4(70)
      endif

      call GET_ENVIRONMENT_VARIABLE('CmdLine' // ' ' // ' ', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false. .and. .true.)
      if ( (LENGTH .ne.LENGTH)  .or. &
           (STATUS .ne. STATUS))     &
      then
        call zzrcy4(71)
      endif


      END

      INCLUDE 'cmdline.include'


      ! Currently ZZRC only support default int size !
      SUBROUTINE ZZRCY4(RC)
        integer RC
        integer( kind=4) :: RC4

        RC4= RC
        call zzrc( RC4 )

      END SUBROUTINE



