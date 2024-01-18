! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat38 999999999-000000000000000-0000000---4444444444444=34444444"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat38
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat38.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing pointers
!*                             : pointing to derivrd types from defferent modules
!*                             : as arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module m2

        type COMMAND
          character(2049)  :: C
        end type COMMAND

        type(COMMAND),   pointer  ::  PCOMMAND

      end module m2

      module m3

        type  LENGTH
          integer  :: C
        end type  LENGTH

        type(LENGTH),    pointer  ::  PLENGTH

      end module m3

      module m4

        type  STATUS
          integer  :: C
        end type  STATUS

        type(STATUS),    pointer  ::  PSTATUS

     end module m4

      module m5

        type NUMBER
          integer  :: C
        end type NUMBER

        type(NUMBER),    pointer  ::  PNUMBER

      end module m5

      module m6

        type VALUE
          character(2047)  :: C
        end type VALUE

        type(VALUE),     pointer  ::  PVALUE

      end module m6

      module m7

        type NAME
          character(513)  :: C
        end type NAME

        type(NAME),      pointer  ::  PNAME

      end module m7

      module m8

        type TRIM_NAME
          logical  :: C
        end type TRIM_NAME

        type(TRIM_NAME), pointer  ::  PTRIM_NAME

      end module m8

      module m9

        type  ARGCOUNT
          integer  :: C
        end type  ARGCOUNT

        type(ARGCOUNT),  pointer  ::  PARGCOUNT

      end module m9




      PROGRAM fxclat38


      use m2
      use m3
      use m4
      use m5
      use m6
      use m7
      use m8
      use m9


      IMPLICIT NONE



      type(COMMAND),    target ::  COMMAND
      type(LENGTH),     target ::  LENGTH
      type(STATUS),     target ::  STATUS
      type(NUMBER),     target ::  NUMBER
      type(VALUE),      target ::  VALUE
      type(NAME),       target ::  NAME
      type(TRIM_NAME),  target ::  TRIM_NAME
      type(ARGCOUNT),   target ::  ARGCOUNT

      character(2049)          :: CmdLine = 'fxclat38 999999999-000000000000000-0000000---4444444444444=34444444'
      integer                  :: CmdCount, i
      character(2047)          :: Argument


      PCOMMAND    => COMMAND
      PLENGTH     => LENGTH
      PSTATUS     => STATUS
      PNUMBER     => NUMBER
      PVALUE      => VALUE
      PNAME       => NAME
      PTRIM_NAME  => TRIM_NAME
      PARGCOUNT   => ARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 1 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(PCOMMAND%C, PLENGTH%C, PSTATUS%C)

      if ( (TRIM(PCOMMAND%C) .ne. TRIM(CmdLine))  .or. &
           (PLENGTH%C .ne. LEN(TRIM(CmdLine)))    .or. &
           (PSTATUS%C .ne. 0) )                        &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        PNUMBER%C = i
        call GET_COMMAND_ARGUMENT(PNUMBER%C, PVALUE%C, PLENGTH%C, PSTATUS%C)
        call MyGetArg(CmdLine, PNUMBER%C, Argument)
        if ( (TRIM(PVALUE%C) .ne. TRIM(Argument))       .or. &
             (PLENGTH%C      .ne. LEN(TRIM(Argument)))  .or. &
             (PSTATUS%C      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      PNAME%C = 'CmdLine     '
      PTRIM_NAME%C = .true.
      call GET_ENVIRONMENT_VARIABLE(PNAME%C, PVALUE%C, PLENGTH%C, PSTATUS%C, PTRIM_NAME%C)
      if ( (TRIM(PVALUE%C) .ne. TRIM(CmdLine))  .or. &
           (PLENGTH%C .ne. LEN(TRIM(CmdLine)))  .or. &
           (PSTATUS%C .ne. 0))                       &
      then
        error stop 66
      endif



      END

      INCLUDE 'cmdline.include'





