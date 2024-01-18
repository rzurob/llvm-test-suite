! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms01"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms01
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms01.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing argument 
!*                             : out of range (to check if the rest of  intent(out) arguments 
!*                             : are properly set
!*                             : 
!234567890123456789012345678901234567890123456789012345678901234567890

 
      module  MOD

        character(63)    :: COMMAND
        integer      	 :: LENGTH
        integer          :: STATUS
        integer          :: NUMBER
        character(63)    :: VALUE     
        integer          :: ARGCOUNT 

        character(63)    :: NAME      = 'CmdLine     '
        logical          :: TRIM_NAME = .true.
        integer          :: CmdCount  = 0
        character(63)    :: CmdLine   = 'fxclms01'
        
      end module 


      PROGRAM fxclms01

      USE MOD

      IMPLICIT NONE

      integer          :: i
      character(63)    :: Argument

      ! no options on command line
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


      !Get options which do not exist
      call GET_COMMAND_ARGUMENT(CmdCount + 1, VALUE, LENGTH, STATUS)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 65
      endif

      call GET_COMMAND_ARGUMENT( -1, VALUE, LENGTH, STATUS)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 66
      endif


      ! Get AN ENVIRONMENT VARIABLE which does not exist. i.e. "_U_V_W__X_Y_Z_ "
      call GET_ENVIRONMENT_VARIABLE('_U_V_W__X_Y_Z_', VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (LENGTH           .ne. 0)  .or.   &
           (STATUS           .le. 0)  .or.   &
           (len(trim(VALUE)) .ne. 0) )       &
      then
        error stop 67
      endif


      END 
 

      INCLUDE 'cmdline.include'

