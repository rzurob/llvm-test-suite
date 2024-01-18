! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat10 1 a 2 b 3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat10
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat10.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing pointee components
!*                             : of derived type defined in a module as actual arguments 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

 
      module modtype

        type dertype
          sequence
          character(2049)  :: COMMAND
          integer      	   :: LENGTH
          character(4099)  :: STR     ! take spaces 
          integer          :: STATUS
          integer          :: NUMBER
          character(2047)  :: VALUE
          INTEGER          :: ARR(10) ! take spaces 
          character(513)   :: NAME
          logical          :: TRIM_NAME
          integer          :: ARGCOUNT
        end type dertype 
         
      end module modtype


      PROGRAM fxclat10

      use modtype

      IMPLICIT NONE



      character(2049)              :: CmdLine = 'fxclat10 1 a 2 b 3'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd
      common /blk/cmd

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT


      POINTER(PtrCOMMAND, COMMAND)
      POINTER(PtrLENGTH, LENGTH)
      POINTER(PtrSTATUS,STATUS)
      POINTER(PtrNUMBER, NUMBER)
      POINTER(PtrVALUE, VALUE)
      POINTER(PtrNAME, NAME)
      POINTER(PtrTRIM_NAME, TRIM_NAME)
      POINTER(PtrARGCOUNT, ARGCOUNT)

       
      PtrCOMMAND   = LOC(cmd%COMMAND)
      PtrLENGTH    = LOC(cmd%LENGTH)
      PtrSTATUS    = LOC(cmd%STATUS)
      PtrNUMBER    = LOC(cmd%NUMBER)
      PtrVALUE     = LOC(cmd%VALUE)
      PtrNAME      = LOC(cmd%NAME)
      PtrTRIM_NAME = LOC(cmd%TRIM_NAME)
      PtrARGCOUNT  = LOC(cmd%ARGCOUNT)

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 5 ) & 
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

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      END 
 
      INCLUDE 'cmdline.include'


      BLOCK DATA BLOCKDATA
         USE modtype
         type(dertype) cmd        
         common  /blk/cmd

         DATA cmd%STR /'1234567890'/
         DATA cmd%ARR /10*1000/
         DATA cmd%NAME /'CmdLine     '/
         DATA cmd%TRIM_NAME  / .true./

      END BLOCK DATA 
  
        


