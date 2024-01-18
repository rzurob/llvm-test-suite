! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu08 \2, \#, \^,"
! %COMPOPTS:  -qfree=f90  
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu08
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu08.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through entries  
!*                             : in elemental subprograms with allocatable actual arguments
!*                             :  
!*           
!234567890123456789012345678901234567890123456789012345678901234567890

 
      PROGRAM fxclpu08

      IMPLICIT NONE


      character(513),  ALLOCATABLE   :: NAME(:)  
      logical,         ALLOCATABLE   :: TRIM_NAME(:) 
      character(2049), ALLOCATABLE   :: CmdLine(:)  
          
      INTEGER              :: i 


      LOGICAL              :: RESULT(3)

 
      INTERFACE 

        ELEMENTAL FUNCTION ENT_CMD_ARG_COUNT(&
          NAME,                              &
          TRIM_NAME,                         & 
          CmdLine   )
          character(513), INTENT(IN)   :: NAME
          logical, INTENT(IN)          :: TRIM_NAME 
          character(2049), INTENT(IN)  :: CmdLine 
          LOGICAL                      :: ENT_CMD_ARG_COUNT
        END FUNCTION

        ELEMENTAL FUNCTION ENT_GET_COMMAND(  &
          NAME,                              &
          TRIM_NAME,                         & 
          CmdLine   )
          character(513), INTENT(IN)   :: NAME
          logical, INTENT(IN)          :: TRIM_NAME 
          character(2049), INTENT(IN)  :: CmdLine 
          LOGICAL                      :: ENT_GET_COMMAND
        END FUNCTION

        ELEMENTAL FUNCTION ENT_GET_CMD_ARG(  &
          NAME,                              &
          TRIM_NAME,                         & 
          CmdLine   )
          character(513), INTENT(IN)   :: NAME
          logical, INTENT(IN)          :: TRIM_NAME 
          character(2049), INTENT(IN)  :: CmdLine 
          LOGICAL                      :: ENT_GET_CMD_ARG
        END FUNCTION

        ELEMENTAL FUNCTION ENT_GET_ENV_VAR(  &
          NAME,                              &
          TRIM_NAME,                         & 
          CmdLine   )
          character(513), INTENT(IN)   :: NAME
          logical, INTENT(IN)          :: TRIM_NAME 
          character(2049), INTENT(IN)  :: CmdLine 
          LOGICAL                      :: ENT_GET_ENV_VAR
        END FUNCTION


      END INTERFACE

     
      ALLOCATE(NAME(3), TRIM_NAME(3), CmdLine(3))

      DO i = 1, 3

      CmdLine(i)   = 'fxclpu08 \\2, \\#, \\^,'
      NAME(i)      = 'CmdLine   '
      TRIM_NAME(i) = .true.
      END DO


      FORALL (i=1:3)
        RESULT(i) =  ENT_CMD_ARG_COUNT( &
                     NAME(1),           &
                     TRIM_NAME(2),      & 
                     CmdLine(3)         )
      END FORALL
      IF ( ANY(RESULT .eqv. .false.)) ERROR STOP 63


      FORALL (i=1:3)
        RESULT(i) =  ENT_GET_COMMAND  ( &
                     NAME(3),           &
                     TRIM_NAME(1),      & 
                     CmdLine(2)         )
      END FORALL
      IF ( ANY(RESULT .eqv. .false.)) ERROR STOP 64


      FORALL (i=1:3)
        RESULT(i) =  ENT_GET_CMD_ARG  ( &
                     NAME(2),           &
                     TRIM_NAME(3),      & 
                     CmdLine(1)         )
      END FORALL
      IF ( ANY(RESULT .eqv. .false.)) ERROR STOP 65


      FORALL (i=1:3)
        RESULT(i) =  ENT_GET_ENV_VAR( &
                     NAME(2),           &
                     TRIM_NAME(1),      & 
                     CmdLine(3)         )
      END FORALL
      IF ( ANY(RESULT .eqv. .false.)) ERROR STOP 66


      DEALLOCATE(NAME, TRIM_NAME, CmdLine)


      END

 
      ELEMENTAL FUNCTION EF_CMD( &
        NAME,                              &
        TRIM_NAME,                         & 
        CmdLine   )

      character(513),  INTENT(IN)   :: NAME
      logical,         INTENT(IN)   :: TRIM_NAME 
      character(2049), INTENT(IN)   :: CmdLine 

      LOGICAL EF_CMD 
      LOGICAL ENT_CMD_ARG_COUNT
      LOGICAL ENT_GET_COMMAND
      LOGICAL ENT_GET_CMD_ARG
      LOGICAL ENT_GET_ENV_VAR

      character(2049), AUTOMATIC  :: COMMAND
      integer,         AUTOMATIC  :: LENGTH     
      integer,         AUTOMATIC  :: STATUS  
      integer,         AUTOMATIC  :: NUMBER 
      character(2047), AUTOMATIC  :: VALUE  
      integer,         AUTOMATIC  :: ARGCOUNT 
      character(2047), AUTOMATIC  :: Argument


      integer, AUTOMATIC          :: CmdCount
      integer, AUTOMATIC          :: i


      INTERFACE

      PURE SUBROUTINE MyGetArg(CmdLine, Number, Arg)
        character*(*), intent(in)  :: CmdLine
        integer,       intent(out) :: Number 
        character*(*), intent(in)  :: Arg 
      END SUBROUTINE

      END INTERFACE


      EF_CMD = .true.
      RETURN


      ENTRY ENT_CMD_ARG_COUNT( &
        NAME,                              &
        TRIM_NAME,                         & 
        CmdLine   )


      ENT_CMD_ARG_COUNT = .true.

      CmdCount = COMMAND_ARGUMENT_COUNT() 
      if ( CmdCount .ne. 3 ) & 
      then
        ENT_CMD_ARG_COUNT = .false.
        !all zzrc(63)
      endif
      RETURN


      ENTRY ENT_GET_COMMAND( &
        NAME,                              &
        TRIM_NAME,                         & 
        CmdLine   )

      ENT_GET_COMMAND = .true.
      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        ENT_GET_COMMAND = .false.
        !error stop 64
      endif
      RETURN


      ENTRY ENT_GET_CMD_ARG( &
        NAME,                              &
        TRIM_NAME,                         & 
        CmdLine   )

      CmdCount = COMMAND_ARGUMENT_COUNT() 
      ENT_GET_CMD_ARG = .true.

      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          ENT_GET_CMD_ARG = .false.
          !error stop 65
        endif

      END DO
      RETURN


      ENTRY ENT_GET_ENV_VAR(   &
        NAME,              &
        TRIM_NAME,         & 
        CmdLine   )

      ENT_GET_ENV_VAR = .true.

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
        ENT_GET_ENV_VAR = .false.
        !error stop 66
      endif
      RETURN



      END FUNCTION


 
      INCLUDE 'cmdline.include'


