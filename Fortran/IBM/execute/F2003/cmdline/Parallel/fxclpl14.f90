!! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export  CmdLine="fxclpl14 ----fthrthr-reth-rth-ertherth-reth-reth-retherth-rth"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl14
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl14.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through section constructs
!*                             : with actual args as pointer target
!*                         
!234567890123456789012345678901234567890123456789012345678901234567890

 

      PROGRAM fxclpl14

      IMPLICIT NONE

      character(2049), POINTER :: PCOMMAND
      integer,         POINTER :: PLENGTH     
      integer,         POINTER :: PSTATUS  
      integer,         POINTER :: PNUMBER 
      character(2047), POINTER :: PVALUE  
      integer,         POINTER :: PARGCOUNT 
    
      character(2049), TARGET  :: COMMAND
      integer,         TARGET  :: LENGTH     
      integer,         TARGET  :: STATUS  
      integer,         TARGET  :: NUMBER 
      character(2047), TARGET  :: VALUE  
      integer,         TARGET  :: ARGCOUNT 
      
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
 
      COMMON /args0/ArgRec

    !$OMP  PARALLEL            & 
    !$OMP  SHARED(/args0/)     &
    !$OMP  PRIVATE(PCOMMAND)   &
    !$OMP  PRIVATE(PLENGTH)    &
    !$OMP  PRIVATE(PSTATUS)    &
    !$OMP  PRIVATE(PNUMBER)    &
    !$OMP  PRIVATE(PVALUE)     &
    !$OMP  PRIVATE(PARGCOUNT)  &
    !$OMP  PRIVATE(COMMAND)    &
    !$OMP  PRIVATE(LENGTH)     &
    !$OMP  PRIVATE(STATUS)     &
    !$OMP  PRIVATE(NUMBER)     &
    !$OMP  PRIVATE(VALUE)      &
    !$OMP  PRIVATE(ARGCOUNT)   &
    !$OMP  PRIVATE(CmdCount)   &
    !$OMP  PRIVATE(Argument)   &
    !$OMP  PRIVATE(i) 


      PCOMMAND  => COMMAND
      PLENGTH   => LENGTH
      PSTATUS   => STATUS
      PNUMBER   => NUMBER
      PVALUE    => VALUE
      PARGCOUNT => ARGCOUNT 
    


     !$OMP  SECTIONS          


     !$OMP SECTION	
      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 1 ) & 
      then
        error stop 63
      endif

      DO i  = 0, CmdCount
        NUMBER = i
        call GET_COMMAND_ARGUMENT(PNUMBER, PVALUE, PLENGTH, PSTATUS)
        call MyGetArg(ArgRec%CmdLine, PNUMBER, Argument)
        if ( (TRIM(PVALUE) .ne. TRIM(Argument))       .or. &
             (PLENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (PSTATUS      .ne. 0) )                       &
        then
          error stop 65
        endif
      END DO


     !$OMP SECTION
      call GET_COMMAND(PCOMMAND, PLENGTH, PSTATUS)
      if ( (TRIM(PCOMMAND) .ne. TRIM(ArgRec%CmdLine))  .or. &
           (PLENGTH .ne. LEN(TRIM(ArgRec%CmdLine)))    .or. &
           (PSTATUS .ne. 0) )                        &
      then
        error stop 64
      endif

     !$OMP SECTION	
      call GET_ENVIRONMENT_VARIABLE(ArgRec%NAME, PVALUE, PLENGTH, PSTATUS, ArgRec%TRIM_NAME)
      if ( (TRIM(PVALUE) .ne. TRIM(ArgRec%CmdLine))  .or. &
            (PLENGTH .ne. LEN(TRIM(ArgRec%CmdLine)))  .or. &
            (PSTATUS .ne. 0))                       &
      then
         error stop 66
      endif


    !$OMP END SECTIONS
    !$OMP END PARALLEL 

      END 


 
      INCLUDE 'cmdline.include'


      BLOCK DATA 

      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      character(2049)  :: CmdLine 
          
      COMMON /args0/CmdLine, NAME, TRIM_NAME

      DATA CmdLine/'fxclpl14 ----fthrthr-reth-rth-ertherth-reth-reth-retherth-rth'/, NAME /'CmdLine   '/, TRIM_NAME /.true./


      END BLOCK DATA






