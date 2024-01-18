! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl11 \?\&"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl11
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl11.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through parallel region within 
!*                             : an external subroutine and actual args are components of derived types 
!*                             : in common block         
!*            
!234567890123456789012345678901234567890123456789012345678901234567890

 

      PROGRAM fxclpl11

      IMPLICIT NONE
      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE  
      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      integer          :: ARGCOUNT 

      character(2049)      :: CmdLine 
          
      COMMON /args/CmdLine, NAME, TRIM_NAME
      

      CmdLine  = 'fxclpl11 \\?\\&'
      NAME = 'CmdLine   '
      TRIM_NAME = .true.

      CALL SUB
     
      END


      SUBROUTINE SUB()

      IMPLICIT NONE

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE  
      integer          :: ARGCOUNT 
          
      TYPE COM
        sequence
        character(2049)  :: CmdLine 
        character(513)   :: NAME  
        logical          :: TRIM_NAME 
      END TYPE

      TYPE(COM)          :: ArgRec

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i
 
      COMMON /args/ArgRec

    !$OMP  PARALLEL          &
    !$OMP  SHARED(/args/)    &
    !$OMP  PRIVATE(COMMAND)  &
    !$OMP  PRIVATE(LENGTH)   &
    !$OMP  PRIVATE(STATUS)   &
    !$OMP  PRIVATE(NUMBER)   &
    !$OMP  PRIVATE(VALUE)    &
    !$OMP  PRIVATE(ARGCOUNT) &
    !$OMP  PRIVATE(Argument) &
    !$OMP  PRIVATE(i) 
	
      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 1 ) & 
      then
        error stop 63
      endif


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(ArgRec%CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(ArgRec%CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

	
      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(ArgRec%CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO


	
      call GET_ENVIRONMENT_VARIABLE(ArgRec%NAME, VALUE, LENGTH, STATUS, ArgRec%TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(ArgRec%CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(ArgRec%CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif

    !$OMP END PARALLEL 

      END SUBROUTINE
 
      INCLUDE 'cmdline.include'

