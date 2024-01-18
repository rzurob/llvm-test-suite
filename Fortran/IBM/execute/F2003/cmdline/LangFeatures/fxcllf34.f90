! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf34 =-=-=-===-=- ___----- _+_+_+_+_"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf34
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf34.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through entries 
!*                             : of external subroutine with pointees as actual 
!*                             : arguments of these intrinsics
!*   
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      character(2049)  :: CmdLine 
          

      DATA CmdLine    /'fxcllf34 =-=-=-===-=- ___----- _+_+_+_+_'/
      DATA NAME       /'CmdLine   '/
      DATA TRIM_NAME  /.true./


      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE  
      integer          :: ARGCOUNT 


      DATA COMMAND    / '????? '/
      DATA LENGTH     / 1111 /
      DATA STATUS     / 1111 /
      DATA NUMBER     /2222/
      DATA VALUE      / 1*'!'/
      DATA ARGCOUNT   / 0 /

      character(513)   :: PteNAME  
      logical          :: PteTRIM_NAME 
      character(2049)  :: PteCOMMAND
      integer          :: PteLENGTH     
      integer          :: PteSTATUS  
      integer          :: PteNUMBER 
      character(2047)  :: PteVALUE  
      integer          :: PteARGCOUNT 


      POINTER(PtrCOMMAND,    PteCOMMAND)
      POINTER(PtrLENGTH,     PteLENGTH)
      POINTER(PtrSTATUS,     PteSTATUS)
      POINTER(PtrNUMBER,     PteNUMBER)
      POINTER(PtrVALUE,      PteVALUE)
      POINTER(PtrNAME,       PteNAME)
      POINTER(PtrTRIM_NAME,  PteTRIM_NAME)
      POINTER(PtrARGCOUNT,   PteARGCOUNT)


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i


      END MODULE



      PROGRAM fxcllf34

      USE MOD
      IMPLICIT NONE

       
      PtrCOMMAND   = LOC(COMMAND)
      PtrLENGTH    = LOC(LENGTH)
      PtrSTATUS    = LOC(STATUS)
      PtrNUMBER    = LOC(NUMBER)
      PtrVALUE     = LOC(VALUE)
      PtrNAME      = LOC(NAME)
      PtrTRIM_NAME = LOC(TRIM_NAME)
      PtrARGCOUNT  = LOC(ARGCOUNT)


      CALL ENT_COMMAND_ARGUMENT_COUNT                                        

      CALL ENT_GET_COMMAND

      CALL ENT_GET_COMMAND_ARGUMENT

      CALL ENT_GET_ENVIRONMENT_VARIABLE


      END


      SUBROUTINE INT_SUB

      USE MOD


      ENTRY ENT_COMMAND_ARGUMENT_COUNT
         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 3 ) & 
         then
           error stop 63
         endif
         RETURN

      ENTRY ENT_GET_COMMAND
        call GET_COMMAND(PteCOMMAND, PteLENGTH, PteSTATUS)
        if ( (TRIM(PteCOMMAND) .ne. TRIM(CmdLine))  .or. &
             (PteLENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (PteSTATUS .ne. 0) )                        &
        then
          error stop 64
        endif
         RETURN

      ENTRY ENT_GET_COMMAND_ARGUMENT
        DO i  = 0, CmdCount
          PteNUMBER = i
          call GET_COMMAND_ARGUMENT(PteNUMBER, PteVALUE, PteLENGTH, PteSTATUS)
          call MyGetArg(CmdLine, PteNUMBER, Argument)
 
          if ( (TRIM(PteVALUE) .ne. TRIM(Argument))       .or. &
               (PteLENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (PteSTATUS      .ne. 0) )                       &
          then
            error stop 65
          endif
        END DO
        RETURN


      ENTRY ENT_GET_ENVIRONMENT_VARIABLE
        call GET_ENVIRONMENT_VARIABLE(PteNAME, PteVALUE, PteLENGTH, PteSTATUS, PteTRIM_NAME)
        if ( (TRIM(PteVALUE) .ne. TRIM(CmdLine))  .or. &
             (PteLENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (PteSTATUS .ne. 0))                       &
        then
          error stop 66
        endif
        RETURN

      END SUBROUTINE




      INCLUDE 'cmdline.include'

