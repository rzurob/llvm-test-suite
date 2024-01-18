! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat17 \[\*\?\]"
! %COMPOPTS:  -qfree=f90 -qintsize=8
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat17
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat17.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Sept 18, 2003
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
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
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Tests command line intrinsic routines by passing  parameters
!*                             : as intent(in) actual arguments and intsize=8
!*                             : 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        type dertype
          sequence
          character(2049)             :: COMMAND
          integer      	              :: LENGTH
          character(4099)             :: STR = '1234567890'
          integer                     :: STATUS
          integer                     :: NUMBER
          character(2047)             :: VALUE
          INTEGER                     :: ARR(10) 
          integer                     :: ARGCOUNT
        end type dertype 

        character(513), PARAMETER   :: NAME = 'CmdLine     '
        logical,        PARAMETER   :: TRIM_NAME = .true.

      end module modtype


      PROGRAM fxclat17

      use modtype

      IMPLICIT NONE


      character(4099) 	STR
      INTEGER         	ARR(10) 

      character(2049)              :: CmdLine = 'fxclat17 \\\[\\\*\\\?\\\]'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd
      common /blk/cmd


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 1 ) & 
      then
        call zzrcY4(63)
      endif

      call GET_COMMAND(cmd%COMMAND, cmd%LENGTH, cmd%STATUS)
      call GET_COMMAND()
      if ( (TRIM(cmd%COMMAND) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%STATUS .ne. 0) )                        &
      then
        call zzrcY4(64)
      endif

      DO i  = 0, CmdCount
       
        cmd%NUMBER = i
        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE, cmd%LENGTH, cmd%STATUS)
        call GET_COMMAND_ARGUMENT(cmd%NUMBER)
        call MyGetArg(CmdLine, cmd%NUMBER, Argument)

        if ( (TRIM(cmd%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          call zzrcY4(65)
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE(NAME, cmd%VALUE, cmd%LENGTH, cmd%STATUS, TRIM_NAME)
      if ( (TRIM(cmd%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (cmd%STATUS .ne. 0))                       &
      then
        call zzrcY4(66)
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






