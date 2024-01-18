! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat11 1 a 2 b"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat11
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat11.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing components of multiple levels
!*                             : of derived type  as actual arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module modtype

        type level3
          sequence
          integer          :: STATUS
          integer          :: NUMBER
          character(2047)  :: VALUE
          INTEGER          :: ARR(10) ! take spaces
          character(513)   :: NAME
          logical          :: TRIM_NAME
          integer          :: ARGCOUNT
        end type

        type level2
          sequence
          integer          :: LENGTH
          character(4099)  :: STR     ! take spaces
          type(level3)     :: l3
        end type

        type level1
          sequence
          character(2049)  :: COMMAND
          type(level2)     :: l2
        end type

      end module modtype


      PROGRAM fxclat11

      USE modtype
      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat11 1 a 2 b'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(level1) cmd
      common /blk/cmd


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(cmd%COMMAND, cmd%l2%LENGTH, cmd%l2%l3%STATUS)
      if ( (TRIM(cmd%COMMAND) .ne. TRIM(CmdLine))     .or. &
           (cmd%l2%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%l2%l3%STATUS .ne. 0) )                     &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        cmd%l2%l3%NUMBER = i
        call GET_COMMAND_ARGUMENT(cmd%l2%l3%NUMBER, cmd%l2%l3%VALUE, cmd%l2%LENGTH, cmd%l2%l3%STATUS)
        call MyGetArg(CmdLine, cmd%l2%l3%NUMBER, Argument)

        if ( (TRIM(cmd%l2%l3%VALUE) .ne. TRIM(Argument))    .or. &
             (cmd%l2%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%l2%l3%STATUS      .ne. 0) )                    &
        then
          error stop 65
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE(cmd%l2%l3%NAME, cmd%l2%l3%VALUE, cmd%l2%LENGTH, cmd%l2%l3%STATUS, cmd%l2%l3%TRIM_NAME)
      if ( (TRIM(cmd%l2%l3%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd%l2%LENGTH .ne. LEN(TRIM(CmdLine)))     .or. &
           (cmd%l2%l3%STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      END

      INCLUDE 'cmdline.include'


      BLOCK DATA BLOCKDATA

        use modtype

        type(level1) cmd
        common  /blk/cmd

        DATA cmd%l2%STR /'1234567890'/
        DATA cmd%l2%l3%ARR /10*1000/
        DATA cmd%l2%l3%NAME /'CmdLine     '/
        DATA cmd%l2%l3%TRIM_NAME  / .true./

      END BLOCK DATA


