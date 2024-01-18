! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat28 OOOOO 000000 llllllllllll 111111111"
! %COMPOPTS:  -qfree=f90 -qintsize=8
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat28
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat28.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing integer
!*                             : pointees of derived type and set integer default size=8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module modtype

        type dertype
          sequence
          character(2049), ALLOCATABLE  :: COMMAND
          integer,         ALLOCATABLE  :: LENGTH
          integer,         ALLOCATABLE  :: STATUS
          integer,         ALLOCATABLE  :: NUMBER
          character(2047), ALLOCATABLE  :: VALUE
          character(513),  ALLOCATABLE  :: NAME
          logical,         ALLOCATABLE  :: TRIM_NAME
          integer,         ALLOCATABLE  :: ARGCOUNT
        end type dertype

      end module modtype


      PROGRAM fxclat28

      use modtype

      IMPLICIT NONE

      character(2049) :: COMMAND
      integer         :: LENGTH
      integer         :: STATUS
      integer         :: NUMBER
      character(2047) :: VALUE
      character(513)  :: NAME
      logical         :: TRIM_NAME
      integer         :: ARGCOUNT


      character(2049)       :: CmdLine = 'fxclat28 OOOOO 000000 llllllllllll 111111111'
      integer               :: CmdCount, i
      character(2047)       :: Argument

      type(dertype) cmd

      POINTER(PCOMMAND,   COMMAND)
      POINTER(PLENGTH,    LENGTH)
      POINTER(PSTATUS,    STATUS)
      POINTER(PNUMBER,    NUMBER)
      POINTER(PVALUE,     VALUE)
      POINTER(PNAME,      NAME)
      POINTER(PTRIM_NAME, TRIM_NAME)
      POINTER(PARGCOUNT,  ARGCOUNT)


      allocate (cmd%COMMAND, cmd%LENGTH, cmd%STATUS, cmd%NUMBER, cmd%VALUE, cmd%NAME, cmd%TRIM_NAME, cmd%ARGCOUNT)

      PCOMMAND   = LOC(cmd%COMMAND)
      PLENGTH    = LOC(cmd%LENGTH)
      PSTATUS    = LOC(cmd%STATUS)
      PNUMBER    = LOC(cmd%NUMBER)
      PVALUE     = LOC(cmd%VALUE)
      PNAME      = LOC(cmd%NAME)
      PTRIM_NAME = LOC(cmd%TRIM_NAME)
      PARGCOUNT  = LOC(cmd%ARGCOUNT)

      if  (.not.allocated(cmd%COMMAND)    .or. .not.allocated(cmd%LENGTH) .or. &
           .not.allocated(cmd%STATUS)     .or. .not.allocated(cmd%NUMBER) .or. &
           .not.allocated(cmd%VALUE)      .or. .not.allocated(cmd%NAME)   .or. &
           .not.allocated(cmd%TRIM_NAME)  .or. .not.allocated(cmd%ARGCOUNT))   &
      then
        call zzrcy4(62)
      endif


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) &
      then
        call zzrcy4(63)
      endif
      call GET_COMMAND(cmd%COMMAND, cmd%LENGTH, cmd%STATUS)
      if ( (TRIM(cmd%COMMAND) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%STATUS .ne. 0) )                        &
      then
        call zzrcy4(64)
      endif

      DO i  = 0, CmdCount

        cmd%NUMBER = i
        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE, cmd%LENGTH, cmd%STATUS)
        call MyGetArg(CmdLine, cmd%NUMBER, Argument)
        if ( (TRIM(cmd%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          call zzrcy4(65)
        endif

      END DO

      cmd%NAME = 'CmdLine     '
      cmd%TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(cmd%NAME, cmd%VALUE, cmd%LENGTH, cmd%STATUS, cmd%TRIM_NAME)
      if ( (TRIM(cmd%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (cmd%STATUS .ne. 0))                       &
      then
        call zzrcy4(66)
      endif

      deallocate (cmd%COMMAND, cmd%LENGTH, cmd%STATUS, cmd%NUMBER, cmd%VALUE, cmd%NAME, cmd%TRIM_NAME, cmd%ARGCOUNT)


      if(allocated(cmd%COMMAND)   .or. allocated(cmd%LENGTH) .or. &
         allocated(cmd%STATUS)    .or. allocated(cmd%NUMBER) .or. &
         allocated(cmd%VALUE)     .or. allocated(cmd%NAME)   .or. &
         allocated(cmd%TRIM_NAME) .or. allocated(cmd%ARGCOUNT))   &
      then
        call zzrcy4(68)
      endif

      END

      INCLUDE 'cmdline.include'

      ! Currently ZZRC only support default int size !
      SUBROUTINE ZZRCY4(RC)
        integer RC
        integer( kind=4) :: RC4

        RC4= RC
        call zzrc( RC4 )

      END SUBROUTINE





