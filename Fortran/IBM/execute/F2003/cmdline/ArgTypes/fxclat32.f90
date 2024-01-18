!#! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat32 _______ --------"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat32
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat131.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing array element pointees 
!*                             : of derived type and COMMAND_ARGUMENT_COUNT as actual arguments 
!*                             : through a call chain
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

        character(2049)              :: CmdLine = 'fxclat32 _______ --------'
        integer                      :: CmdCount = 2
        integer                      :: i
        character(2047)              :: Argument
         
         type(dertype) cmd(8)

      end module modtype


      PROGRAM fxclat132

      use modtype

      IMPLICIT NONE

      INTRINSIC COMMAND_ARGUMENT_COUNT         



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

       
      PtrCOMMAND   = LOC(cmd(1)%COMMAND)
      PtrLENGTH    = LOC(cmd(2)%LENGTH)
      PtrSTATUS    = LOC(cmd(3)%STATUS)
      PtrNUMBER    = LOC(cmd(4)%NUMBER)
      PtrVALUE     = LOC(cmd(5)%VALUE)
      PtrNAME      = LOC(cmd(6)%NAME)
      PtrTRIM_NAME = LOC(cmd(7)%TRIM_NAME)
      PtrARGCOUNT  = LOC(cmd(8)%ARGCOUNT)

      NAME = 'CmdLine  '
      TRIM_NAME = .true.

      CALL SUB1(COMMAND_ARGUMENT_COUNT, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)

      CALL SUB2(COMMAND_ARGUMENT_COUNT, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)
 
      CALL SUB3(COMMAND_ARGUMENT_COUNT, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)

      CONTAINS


      SUBROUTINE  SUB3(FUN, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)
      character(*)     :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(*)     :: VALUE
      character(*)     :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      INTEGER, EXTERNAL:: FUN

        CALL SUB2(FUN, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)

      ENDSUBROUTINE


      END 


      INCLUDE 'cmdline.include'



      SUBROUTINE  SUB1(FUN, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)

      use modtype
      character(*)     :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(*)     :: VALUE
      character(*)     :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT
      
      integer          :: FUN 

      if ( FUN().ne. CmdCount ) & 
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

      END SUBROUTINE

      SUBROUTINE SUB2(FUN, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)
      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      INTEGER, EXTERNAL:: FUN

        CALL SUB1(FUN, COMMAND, LENGTH, STATUS, NUMBER,  VALUE, NAME, TRIM_NAME, ARGCOUNT)

      END SUBROUTINE


