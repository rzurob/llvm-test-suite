! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat25 1 a 2"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat25
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat25.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing string sections of variables defined
!*                             : in different derived types as the augument keywords
!*                             :
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

         type C
           character(2049)      :: COMMAND
         end type C

         type L
           integer              :: LENGTH
         end type L

         type S
           integer              :: STATUS
         end type S

         type Nu
           integer              :: NUMBER
         end type Nu

         type V
           character(2047)      :: VALUE
         end type V

         type Na
           character(513)       :: NAME
         end type Na

         type T
           logical              :: TRIM_NAME
         end type

         type A
           integer              :: ARGCOUNT
         end type A

      end module modtype


      PROGRAM fxclat25

      use modtype

      IMPLICIT NONE


      type(C)  C
      type(L)  L
      type(S)  S
      type(Nu) Nu
      type(V)  V
      type(Na) Na
      type(T)  T
      type(A)  A


      character(2049)              :: CmdLine = 'fxclat25 1 a 2'
      integer                      :: CmdCount, i
      character(2047)              :: Argument
      integer LENGTH
      integer STATUS

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND=C%COMMAND(33:532), LENGTH=L%LENGTH, STATUS=S%STATUS)
      call GET_COMMAND()
      if ( (TRIM(C%COMMAND(33:532)) .ne. TRIM(CmdLine))  .or. &
           (L%LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (S%STATUS .ne. 0) )                                &
      then
        error stop 64
      endif

      call GET_COMMAND(COMMAND=C%COMMAND(1:101), LENGTH=L%LENGTH)
      if ( (TRIM(C%COMMAND(1:101)) .ne. TRIM(CmdLine))  .or. &
           (L%LENGTH .ne. LEN(TRIM(CmdLine))))               &
      then
        error stop 65
      endif

      call GET_COMMAND(COMMAND=C%COMMAND(1001:2049))
      if ( TRIM(C%COMMAND(1001:2049)) .ne. TRIM(CmdLine))  &
      then
        error stop 66
      endif


      DO i  = 0, CmdCount

        Nu%NUMBER = i
        call MyGetArg(CmdLine, Nu%NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(NUMBER=Nu%NUMBER, VALUE=V%VALUE(1023:2046), LENGTH=L%LENGTH,STATUS=S%STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER=Nu%NUMBER)

        if ( (TRIM(V%VALUE(1023:2046)) .ne. TRIM(Argument))     .or. &
             (L%LENGTH      .ne. LEN(TRIM(Argument)))           .or. &
             (S%STATUS      .ne. 0) )                                &
        then
          error stop 67
        endif

        call GET_COMMAND_ARGUMENT(NUMBER=Nu%NUMBER, VALUE=V%VALUE(513:688), LENGTH=L%LENGTH)
        if ( (TRIM(V%VALUE(513:688)) .ne. TRIM(Argument))     .or. &
             (L%LENGTH      .ne. LEN(TRIM(Argument))))             &
        then
          error stop 68
        endif

        call GET_COMMAND_ARGUMENT(Nu%NUMBER, VALUE =V%VALUE(11:511), STATUS=S%STATUS)
        if ( (TRIM(V%VALUE(11:511)) .ne. TRIM(Argument))  .or. &
             (S%STATUS      .ne. 0) )                          &
        then
          error stop 69
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', VALUE=V%VALUE(1013:2039), LENGTH=L%LENGTH, STATUS=S%STATUS, TRIM_NAME=.true.)
      if ( (TRIM(V%VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (L%LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (S%STATUS .ne. 0))                                  &
      then
        error stop 70
      endif

      call GET_ENVIRONMENT_VARIABLE('CmdLine', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false.)
      if ( (LENGTH .ne.L%LENGTH)  .or. &
           (STATUS.ne. S%STATUS))      &
      then
        error stop 71
      endif


      END


      INCLUDE 'cmdline.include'





