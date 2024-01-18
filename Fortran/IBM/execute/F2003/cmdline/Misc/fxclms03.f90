! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms03 -= -- -0 -1 -a"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms03
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms03.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing short strings as
!*                             : actual arguments. 
!*                             : (to check if these intrinsic routine can handle those smallest size args)
!*                             : (It is short of storage for any options) 
!*                             : 
!*                             : 
!234567890123456789012345678901234567890123456789012345678901234567890

 
      module  MOD

        character(1)    :: COMMAND
        integer         :: LENGTH
        integer         :: STATUS
        integer         :: NUMBER
        character(1)    :: VALUE     
        integer         :: ARGCOUNT 

        logical         :: TRIM_NAME = .true.
        integer         :: CmdCount  = 5
        character(7)    :: NAME      = 'CmdLine'
        character(64)   :: CmdLine   = 'fxclms03 -= -- -0 -1 -a'
    
       
      end module 


      PROGRAM fxclms02

      USE MOD

      IMPLICIT NONE


      integer          :: i
      character(64)    :: Argument

 
 
      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif

      
      call GET_COMMAND(COMMAND, LENGTH, STATUS)

      if ( (TRIM(COMMAND) .ne. 'f'          )  .or. &  
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. -1) )                       &
      then
        error stop 64
      endif
     ! The first char on command line is "f"
     ! According to spec, STATUS should be -1 when truncation occurs


      DO NUMBER = 0, CmdCount

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( ((TRIM(VALUE) .ne. 'f') .and. (TRIM(VALUE) .ne. '-') ) .or. &
             (LENGTH       .le. 1 )                                 .or. &
             (STATUS       .ne. -1) )                                    &
        then
          error stop 65
        endif
        ! The first char of each option is "f" or "-"
        ! According to spec, STATUS should be -1 when truncation occurs

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

      if ( (TRIM(VALUE) .ne. 'f')  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. -1))                      &
      then
        error stop 68
      endif
     ! The first char in the environment variable is "f"



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

