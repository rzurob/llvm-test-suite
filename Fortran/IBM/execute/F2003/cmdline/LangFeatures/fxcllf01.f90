! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf01 1 I 3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf01
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf01.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing components
!*                             : of  multiple levels of derived types initialized with structure constructor 
!*                             : as actual arguments
!*
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

      module modtype

        type level3
          sequence
          integer          :: STATUS
          integer          :: NUMBER
          character(2047)  :: VALUE
          integer          :: LENGTH
          integer          :: ARGCOUNT
        end type

        type level2
          sequence
          character(2049)              :: CmdLine
          character(513)                :: NAME 
          logical                       :: TRIM_NAME 
          type(level3)     :: l3
        end type

        type level1
          sequence
          character(2049)  :: COMMAND
          type(level2)     :: l2
        end type
         
      end module modtype


      PROGRAM fxcllf01
      
      USE modtype
      IMPLICIT NONE


      character(2049)              :: CmdLine 
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(level1) ::  cmd
 
     cmd = level1('???  ', level2( 'fxcllf01 1 I 3', 'CmdLine   ', .true., level3(1, 2, 'xxxx', 3, 4 )  )  )

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) & 
      then
        error stop 63
      endif

      call GET_COMMAND()
      call GET_COMMAND(cmd%COMMAND, cmd%l2%l3%LENGTH, cmd%l2%l3%STATUS)
      if ( (TRIM(cmd%COMMAND) .ne. TRIM(cmd%l2%CmdLine))     .or. &
           (cmd%l2%l3%LENGTH .ne. LEN(TRIM(cmd%l2%CmdLine)))    .or. &
           (cmd%l2%l3%STATUS .ne. 0) )                     &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount
       
        cmd%l2%l3%NUMBER = i
        call GET_COMMAND_ARGUMENT(cmd%l2%l3%NUMBER)
        call GET_COMMAND_ARGUMENT(cmd%l2%l3%NUMBER, cmd%l2%l3%VALUE, cmd%l2%l3%LENGTH, cmd%l2%l3%STATUS)
        call MyGetArg(cmd%l2%CmdLine, cmd%l2%l3%NUMBER, Argument)

        if ( (TRIM(cmd%l2%l3%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%l2%l3%LENGTH         .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%l2%l3%STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE(cmd%l2%NAME, cmd%l2%l3%VALUE, cmd%l2%l3%LENGTH, cmd%l2%l3%STATUS, cmd%l2%TRIM_NAME)
      if ( (TRIM(cmd%l2%l3%VALUE) .ne. TRIM(cmd%l2%CmdLine))  .or. &
           (cmd%l2%l3%LENGTH .ne. LEN(TRIM(cmd%l2%CmdLine)))     .or. &
           (cmd%l2%l3%STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      END 
 
      INCLUDE 'cmdline.include'

  

