! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf35 :::::::::::::::::::::::::::::: :1 :2 :3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf35
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf35.f
!*
!*  DATE                       : Sept 18, 2003
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
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Call command line intrinsic routines with various aliases as actual
!*                             : arguments (  through combination of equivalence, pointer and pointee )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME, EqNAME
      logical          :: TRIM_NAME, EqTRIM_NAME
      character(2049)  :: CmdLine


      DATA CmdLine    /'fxcllf35 :::::::::::::::::::::::::::::: :1 :2 :3'/
      DATA EqNAME       /'CmdLine   '/
      DATA EqTRIM_NAME  /.true./

      EQUIVALENCE ( NAME, EqNAME)
      EQUIVALENCE ( TRIM_NAME, EqTRIM_NAME)



      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047), TARGET  :: VALUE
      integer          :: ARGCOUNT


      DATA COMMAND    / '????? '/
      DATA LENGTH     / 1111 /
      DATA STATUS     / 1111 /
      DATA NUMBER     /2222/
      DATA VALUE      / 1*'!'/
      DATA ARGCOUNT   / 0 /


      character(2049)           :: PteCOMMAND
      integer                   :: PteLENGTH
      integer                   :: PteSTATUS
      integer                   :: PteNUMBER
      character(2047), POINTER  :: PtrVALUE
      integer                   :: PteARGCOUNT


      POINTER(PtrCOMMAND,    PteCOMMAND)
      POINTER(PtrLENGTH,     PteLENGTH)
      POINTER(PtrSTATUS,     PteSTATUS)
      POINTER(PtrNUMBER,     PteNUMBER)
      POINTER(PtrARGCOUNT,   PteARGCOUNT)


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i


      END MODULE



      PROGRAM fxcllf35

      USE MOD
      IMPLICIT NONE


      PtrCOMMAND   = LOC(COMMAND)
      PtrLENGTH    = LOC(LENGTH)
      PtrSTATUS    = LOC(STATUS)
      PtrNUMBER    = LOC(NUMBER)
      PtrARGCOUNT  = LOC(ARGCOUNT)

      PtrVALUE => VALUE


      CALL ENT_COMMAND_ARGUMENT_COUNT

      CALL ENT_GET_COMMAND

      CALL ENT_GET_COMMAND_ARGUMENT

      CALL ENT_GET_ENVIRONMENT_VARIABLE


      END


      SUBROUTINE INT_SUB

      USE MOD


      ENTRY ENT_COMMAND_ARGUMENT_COUNT
         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 4 ) &
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
          call GET_COMMAND_ARGUMENT(PteNUMBER, PtrVALUE, PteLENGTH, PteSTATUS)
          call MyGetArg(CmdLine, PteNUMBER, Argument)

          if ( (TRIM(PtrVALUE) .ne. TRIM(Argument))       .or. &
               (PteLENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (PteSTATUS      .ne. 0) )                       &
          then
            error stop 65
          endif
        END DO
        RETURN


      ENTRY ENT_GET_ENVIRONMENT_VARIABLE
        call GET_ENVIRONMENT_VARIABLE(EqNAME, PtrVALUE, PteLENGTH, PteSTATUS, EqTRIM_NAME)
        if ( (TRIM(PtrVALUE) .ne. TRIM(CmdLine))  .or. &
             (PteLENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (PteSTATUS .ne. 0))                       &
        then
          error stop 66
        endif
        RETURN

      END SUBROUTINE




      INCLUDE 'cmdline.include'

