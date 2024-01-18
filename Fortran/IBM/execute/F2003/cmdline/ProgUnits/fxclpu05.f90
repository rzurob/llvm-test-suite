! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Call command line procedures through external subroutine
!*                             : with actual arguments defined in derived type in module and
!*                             : with allocatable attribute
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      TYPE DT
      character(2049), ALLOCATABLE   :: COMMAND
      integer,         ALLOCATABLE   :: LENGTH
      integer,         ALLOCATABLE   :: STATUS
      integer,         ALLOCATABLE   :: NUMBER
      character(2047), ALLOCATABLE   :: VALUE
      character(513),  ALLOCATABLE   :: NAME
      logical,         ALLOCATABLE   :: TRIM_NAME
      integer,         ALLOCATABLE   :: ARGCOUNT
      END TYPE

      END MODULE


      PROGRAM fxclpu05
      USE MOD
      IMPLICIT NONE


      TYPE(DT) cmd

      INTERFACE
        SUBROUTINE EXT_SUB(    &
                    COMMAND,   &
                    LENGTH,    &
                    STATUS,    &
                    NUMBER,    &
                    VALUE  ,   &
                    NAME  ,    &
                    TRIM_NAME ,&
                    ARGCOUNT   )

          character(2049), ALLOCATABLE   :: COMMAND
          integer,         ALLOCATABLE   :: LENGTH
          integer,         ALLOCATABLE   :: STATUS
          integer,         ALLOCATABLE   :: NUMBER
          character(2047), ALLOCATABLE   :: VALUE
          character(513),  ALLOCATABLE   :: NAME
          logical,         ALLOCATABLE   :: TRIM_NAME
          integer,         ALLOCATABLE   :: ARGCOUNT

        END SUBROUTINE
      END INTERFACE


      CALL EXT_SUB(            &
                    cmd%COMMAND,   &
                    cmd%LENGTH,    &
                    cmd%STATUS,    &
                    cmd%NUMBER,    &
                    cmd%VALUE  ,   &
                    cmd%NAME  ,    &
                    cmd%TRIM_NAME ,&
                    cmd%ARGCOUNT   )



      END

      SUBROUTINE EXT_SUB(  &
                          COMMAND,   &
                          LENGTH,    &
                          STATUS,    &
                          NUMBER,    &
                          VALUE,     &
                          NAME,      &
                          TRIM_NAME, &
                          ARGCOUNT   )

      character(2049), ALLOCATABLE   :: COMMAND
      integer,         ALLOCATABLE   :: LENGTH
      integer,         ALLOCATABLE   :: STATUS
      integer,         ALLOCATABLE   :: NUMBER
      character(2047), ALLOCATABLE   :: VALUE
      character(513) , ALLOCATABLE   :: NAME
      logical,         ALLOCATABLE   :: TRIM_NAME
      integer,         ALLOCATABLE   :: ARGCOUNT

      character(2049)              :: CmdLine = 'fxclpu05 1 a'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      ALLOCATE(                      &
                          COMMAND,   &
                          LENGTH,    &
                          STATUS,    &
                          NUMBER,    &
                          VALUE,     &
                          NAME,      &
                          TRIM_NAME, &
                          ARGCOUNT   )


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 2 ) &
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


      NAME = 'CmdLine     '
      TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      DEALLOCATE(                    &
                          COMMAND,   &
                          LENGTH,    &
                          STATUS,    &
                          NUMBER,    &
                          VALUE,     &
                          NAME,      &
                          TRIM_NAME, &
                          ARGCOUNT   )
      END SUBROUTINE


      INCLUDE 'cmdline.include'


