! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: unset _U_V_W__X_Y_Z_;  export CmdLine="fxclms04 1 a 2 b 3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms04
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms04.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct 1, 2003
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing long strings as
!*                             : actual arguments
!*                             : Define these arguments in common block
!*                             : (to check if these intrinsic routine can handle those big size args)
!*                             : (Currently the maximum length for command line on all platform is 
!*                             : less than 256k = 262144)
!*                             : 
!234567890123456789012345678901234567890123456789012345678901234567890

 
      module  MOD

        character(262144)    :: COMMAND
        integer      	     :: LENGTH
        integer              :: STATUS
        integer              :: NUMBER
        character(262144)    :: VALUE     
        integer              :: ARGCOUNT 

        character(262144)    :: NAME      
        logical              :: TRIM_NAME 
        integer              :: CmdCount  
        character(262144)    :: CmdLine 
        character(262144)    :: CmdLine1 


        COMMON /com/COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT, NAME, &
                  TRIM_NAME, CmdCount, CmdLine

      end module 


      PROGRAM fxclms04

      USE MOD

      IMPLICIT NONE


      integer              :: i
      character(131072)    :: Argument

      NAME       = 'CmdLine     '
      CmdLine    =  'fxclms04 1 a 2 b 3'
      CmdLine1   =  'fxclms04 1 a 2 b 3'
      TRIM_NAME  = .true.
      CmdCount   = 5
 
 
      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif

      
      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


      DO NUMBER = 0, CmdCount

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      !Get options which do not exist
      call GET_COMMAND_ARGUMENT(CmdCount + 1, VALUE, LENGTH, STATUS)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 66
      endif

      !Get options which do not exist
      call GET_COMMAND_ARGUMENT( -1, VALUE, LENGTH, STATUS)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 67
      endif



      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)

      if ( (TRIM(VALUE) .ne. TRIM(CmdLine1))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine1)))  .or. &
           (STATUS .ne. 0))                        &
      then
        error stop 68
      endif

      ! Get AN ENVIRONMENT VARIABLE which does not exist. i.e. "_U_V_W__X_Y_Z_ "
      call GET_ENVIRONMENT_VARIABLE('_U_V_W__X_Y_Z_', VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 69
      endif


      END 
 

      INCLUDE 'cmdline.include'

