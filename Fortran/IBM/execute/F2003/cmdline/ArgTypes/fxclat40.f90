! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat40 \$aaaaaaaaaaaaa \$bbbbbbbbbbbbbbbbb"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat40
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat40.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing elements of
!*                             : pointer array pointing to various variables
!*                             : as arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module m1

        type STR
          character(2049), pointer  :: P
        end type STR

      end module m1

      module m2

        type  INT
          integer, pointer  :: P
        end type INT

      end module m2

      module m3

        type LOGIC
          logical, pointer  :: P
        end type LOGIC

      end module m3


      PROGRAM fxclat40


      use m2
      use m3
      use m1



      IMPLICIT NONE

      type(STR),    pointer  ::  PSTR(:)
      type(INT),    pointer  ::  PINT(:)
      type(LOGIC),  pointer  ::  PLOGIC(:)

      type(STR),   target    ::  STR(3)
      type(INT),   target    ::  INT(4)
      type(LOGIC), target    ::  LOGIC(1)

      character(2049)        ::  CmdLine = 'fxclat40 $aaaaaaaaaaaaa $bbbbbbbbbbbbbbbbb'
      integer                ::  CmdCount, i
      character(2047)        ::  Argument

      character(2049), target :: COMMAND
      integer,         target :: LENGTH
      integer,         target :: STATUS
      integer,         target :: NUMBER
      character(2049), target :: VALUE
      character(2049), target :: NAME
      logical,         target :: TRIM_NAME
      integer,         target :: ARGCOUNT

      PSTR    => STR
      PINT   => INT
      PLOGIC => LOGIC


      PSTR(1)%P    => COMMAND
      PINT(1)%P    => LENGTH
      PINT(2)%P    => STATUS
      PINT(3)%P    => NUMBER
      PSTR(2)%P    => VALUE
      PSTR(3)%P    => NAME
      PLOGIC(1)%P  => TRIM_NAME
      PINT(4)%P    => ARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 2 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(PSTR(1)%P, PINT(1)%P, PINT(2)%P)
      if ( (TRIM(PSTR(1)%P) .ne. TRIM(CmdLine))   .or. &
           (PINT(1)%P .lt. LEN(TRIM(CmdLine)))    .or. &
           (PINT(1)%P .gt. LEN(CmdLine))          .or. &
           (PINT(2)%P .gt. 0) )                        &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        PINT(3)%P = i
        call GET_COMMAND_ARGUMENT(PINT(3)%P, PSTR(2)%P, PINT(1)%P, PINT(2)%P)
        call MyGetArg(CmdLine, PINT(3)%P, Argument)
        if ( (TRIM(PSTR(2)%P) .ne. TRIM(Argument))      .or. &
             (PINT(1)%P      .lt. LEN(TRIM(Argument)))  .or. &
             (PINT(1)%P      .gt. LEN(Argument))        .or. &
             (PINT(2)%P      .gt. 0) )                       &
        then
          error stop 65
        endif

      END DO

      PSTR(3)%P = 'CmdLine     '
      PLOGIC(1)%P = .true.
      call GET_ENVIRONMENT_VARIABLE(PSTR(3)%P, PSTR(2)%P, PINT(1)%P, PINT(2)%P, PLOGIC(1)%P)
      if ( (TRIM(PSTR(2)%P) .ne. TRIM(CmdLine))  .or. &
           (PINT(1)%P .lt. LEN(TRIM(CmdLine)))   .or. &
           (PINT(1)%P .gt. LEN(CmdLine))         .or. &
           (PINT(2)%P .gt. 0))                        &
      then
        error stop 66
      endif



      END

      INCLUDE 'cmdline.include'





