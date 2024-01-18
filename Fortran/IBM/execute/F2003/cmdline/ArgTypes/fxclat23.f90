! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat23 \{ \[ \} \]"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat23
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat23.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing array elements
!*                             : (string sections) of derived type with sequence statement
!*                             : as actual arguments
!*                             : 
!*              
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        type dertype
          sequence
          character(2049)             :: COMMAND(3)
          integer      	              :: LENGTH(3)
          character(4099)             :: STR = '1234567890'
          integer                     :: STATUS(3)
          integer                     :: NUMBER(3)
          character(2047)             :: VALUE(3)
          INTEGER                     :: ARR(10) 
          integer                     :: ARGCOUNT(3)
        end type dertype 

      end module modtype


      PROGRAM fxclat23

      use modtype

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat23 \\{ \\[ \\} \\]'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd
      common /blk/cmd 

      character(2049)             :: COMMAND
      integer      	              :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE 
      integer                     :: ARGCOUNT
  


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) & 
      then
        error stop 63
      endif

      call GET_COMMAND(cmd%COMMAND(1)(33:532), cmd%LENGTH(1), cmd%STATUS(1))
      call GET_COMMAND()
      if ( (TRIM(cmd%COMMAND(1)(33:532)) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH(1) .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%STATUS(1) .ne. 0) )                        &
      then
        error stop 64
      endif

      call GET_COMMAND(cmd%COMMAND(2)(1:101), cmd%LENGTH(2))
      if ( (TRIM(cmd%COMMAND(2)(1:101)) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH(2) .ne. LEN(TRIM(CmdLine))))               &
      then
        error stop 65
      endif

      call GET_COMMAND(cmd%COMMAND(3)(1001:2049))
      if ( TRIM(cmd%COMMAND(3)(1001:2049)) .ne. TRIM(CmdLine))  &
      then
        error stop 66
      endif


      DO i  = 0, CmdCount
       
        cmd%NUMBER(2) = i
        call MyGetArg(CmdLine, cmd%NUMBER(2), Argument)

        call GET_COMMAND_ARGUMENT(cmd%NUMBER(2), cmd%VALUE(1)(1023:2046), cmd%LENGTH(1), cmd%STATUS(1))
        call GET_COMMAND_ARGUMENT(cmd%NUMBER(2))

        if ( (TRIM(cmd%VALUE(1)(1023:2046)) .ne. TRIM(Argument))     .or. &
             (cmd%LENGTH(1)      .ne. LEN(TRIM(Argument)))           .or. &
             (cmd%STATUS(1)      .ne. 0) )                                &
        then
          error stop 67
        endif

        call GET_COMMAND_ARGUMENT(cmd%NUMBER(2), cmd%VALUE(3)(513:688), cmd%LENGTH(3))
        if ( (TRIM(cmd%VALUE(3)(513:688)) .ne. TRIM(Argument)) .or. &
             (cmd%LENGTH(3)      .ne. LEN(TRIM(Argument))))         &
        then
          error stop 68
        endif

        call GET_COMMAND_ARGUMENT(cmd%NUMBER(2), VALUE =cmd%VALUE(2)(11:511), STATUS=cmd%STATUS(2))
        if ( (TRIM(cmd%VALUE(2)(11:511)) .ne. TRIM(Argument))  .or. &
             (cmd%STATUS(2)      .ne. 0) )                          &
        then
          error stop 69
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', cmd%VALUE(1)(1013:2039), cmd%LENGTH(1), cmd%STATUS(1), .true.)
      if ( (TRIM(cmd%VALUE(1)(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH(1) .ne. LEN(TRIM(CmdLine)))             .or. &
           (cmd%STATUS(1) .ne. 0))                                  &
      then
        error stop 70
      endif

      call GET_ENVIRONMENT_VARIABLE('CmdLine', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false.)
      if ( (cmd%LENGTH(1) .ne.LENGTH)  .or. &
           (cmd%STATUS(1) .ne. STATUS))     &
      then
        error stop 71
      endif


      END 
 

      INCLUDE 'cmdline.include'





  
        
