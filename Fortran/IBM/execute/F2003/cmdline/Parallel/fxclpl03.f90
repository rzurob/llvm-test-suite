! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl03 _______111111111111 --------11111 ++++++1111111"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl03
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl03.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through single constructs
!*                             : with COPYPRIVATE clause and FIRSTPRIVATE clauses
!*                                      
!*                          
!234567890123456789012345678901234567890123456789012345678901234567890

 

      PROGRAM fxclpl03

      IMPLICIT NONE


      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE  
      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      integer          :: ARGCOUNT 


      character(2049)      :: CmdLine  = 'fxclpl03 _______111111111111 --------11111 ++++++1111111'
      integer              :: CmdCount, i, k
      character(2047)      :: Argument
     
      NAME = '??????  '
      TRIM_NAME = .false.



    !$OMP PARALLEL  &
    !$OMP  FIRSTPRIVATE(CmdLine) &
    !$OMP  PRIVATE(COMMAND) &
    !$OMP  PRIVATE(LENGTH) &
    !$OMP  PRIVATE(STATUS) &
    !$OMP  PRIVATE(NUMBER) &
    !$OMP  PRIVATE(VALUE) &
    !$OMP  PRIVATE(NAME) &
    !$OMP  PRIVATE(TRIM_NAME) &
    !$OMP  PRIVATE(ARGCOUNT) &
    !$OMP  PRIVATE(Argument) 


    !$OMP SINGLE
	
      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) & 
      then
        error stop 63
      endif

      NAME = 'CmdLine    '
      TRIM_NAME = .true.

    !$OMP END SINGLE COPYPRIVATE(NAME, TRIM_NAME)    ! Carry on NAME and TRIM_NAME


    !$OMP SINGLE	   

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

    !$OMP END SINGLE COPYPRIVATE(NAME, TRIM_NAME)    ! Carry on NAME and TRIM_NAME

    !$OMP SINGLE
	
      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

    !$OMP END SINGLE COPYPRIVATE(NAME, TRIM_NAME)    ! Carry on NAME and TRIM_NAME

    !$OMP SINGLE
	
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif

    !$OMP END SINGLE 

    !$OMP END PARALLEL

      END 
 
      INCLUDE 'cmdline.include'

