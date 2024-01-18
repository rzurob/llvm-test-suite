! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat09 fxclat09 fxclat09.f fxclat09.o fxclat09.s fxclat09.c fxclat09.out fxclat09.vf"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat09
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat07.f
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
!*  DESCRIPTION                : Command line procedure arguments are string sections of
!*                             : allocatable variables defined in a module.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module modtype

          character(2049), ALLOCATABLE  :: COMMAND
          integer,         ALLOCATABLE  :: LENGTH
          character(4099), ALLOCATABLE  :: STR
          integer,         ALLOCATABLE  :: STATUS
          integer,         ALLOCATABLE  :: NUMBER
          character(2047), ALLOCATABLE  :: VALUE
          INTEGER,         ALLOCATABLE  :: ARR(:)
          character(513),  ALLOCATABLE  :: NAME
          logical,         ALLOCATABLE  :: TRIM_NAME
          integer,         ALLOCATABLE  :: ARGCOUNT


      end module modtype


      PROGRAM fxclat09


      use modtype,               &
        COMMAND     => COMMAND , &
        LENGTH      => LENGTH  , &
        STATUS      => STATUS  , &
        NUMBER      => NUMBER  , &
        VALUE       => VALUE   , &
        NAME        => NAME    , &
        TRIM_NAME   => TRIM_NAME,&
        ARGCOUNT    => ARGCOUNT


      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat09 fxclat09 fxclat09.f fxclat09.o fxclat09.s fxclat09.c fxclat09.out fxclat09.vf'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

!     type(dertype) cmd

      if(allocated(COMMAND)   .or. allocated(LENGTH) .or. &
         allocated(STATUS)    .or. allocated(NUMBER) .or. &
         allocated(VALUE)     .or. allocated(NAME)   .or. &
         allocated(TRIM_NAME) .or. allocated(ARGCOUNT))   &
      then
        error stop 61
      endif

      allocate (COMMAND, LENGTH, STATUS, NUMBER, VALUE, NAME, TRIM_NAME, ARGCOUNT)

      if  (.not.allocated(COMMAND)    .or. .not.allocated(LENGTH) .or. &
           .not.allocated(STATUS)     .or. .not.allocated(NUMBER) .or. &
           .not.allocated(VALUE)      .or. .not.allocated(NAME)   .or. &
           .not.allocated(TRIM_NAME)  .or. .not.allocated(ARGCOUNT))   &
      then
        error stop 62
      endif


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 7 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND( 127:1049), LENGTH, STATUS)
      if ( (TRIM(COMMAND( 127:1049)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE(1023:2047), LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)
        if ( (TRIM(VALUE(1023:2047)) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      NAME = 'CmdLine     '
      TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE(1:), LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE(1:)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      deallocate (COMMAND, LENGTH, STATUS, NUMBER, VALUE, NAME, TRIM_NAME, ARGCOUNT)

      if(allocated(COMMAND)   .or. allocated(LENGTH) .or. &
         allocated(STATUS)    .or. allocated(NUMBER) .or. &
         allocated(VALUE)     .or. allocated(NAME)   .or. &
         allocated(TRIM_NAME) .or. allocated(ARGCOUNT))   &
      then
        error stop 68
      endif


      END

      INCLUDE 'cmdline.include'





