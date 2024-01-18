! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf21 /a/s/d/f/g/h/ a-s-d-f-g q=w=e=r=t="
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf21
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf21.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through  
!*                             : a call chain of internal / external functions by do loops
!*       
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      character(2049)  :: CmdLine 
          

      DATA CmdLine    /'fxcllf21 /a/s/d/f/g/h/ a-s-d-f-g q=w=e=r=t='/
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



      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i


      END MODULE



      PROGRAM fxcllf21
      IMPLICIT NONE

      INTERFACE

      FUNCTION EXT_COMMAND_ARGUMENT_COUNT()
        LOGICAL EXT_COMMAND_ARGUMENT_COUNT
      END FUNCTION

      FUNCTION EXT_GET_COMMAND()
        LOGICAL EXT_GET_COMMAND
      END FUNCTION

      FUNCTION EXT_GET_COMMAND_ARGUMENT(Num)
        LOGICAL EXT_GET_COMMAND_ARGUMENT
        INTEGER Num
      END FUNCTION


      FUNCTION EXT_GET_ENVIRONMENT_VARIABLE()
        LOGICAL EXT_GET_ENVIRONMENT_VARIABLE
      END FUNCTION


      END INTERFACE

      INTEGER i, j, k

      DO i = 1, 2
      DO j = 2, 3
      DO k =3, 4
        CALL SUB0
      END DO
      END DO
      END DO

      DO i = 2, 1, -1
      DO j = 2, 3
      DO k =4, 3, -1
        CALL SUB0
      END DO
      END DO
      END DO


      CONTAINS

      SUBROUTINE SUB0

      IF ( COMMAND_ARGUMENT_COUNT() .ne. 3)                      call zzrc( 72 ) 
                                                    
      IF ( EXT_COMMAND_ARGUMENT_COUNT( ))                        call zzrc( 73 )     
                                  
      IF ( EXT_GET_COMMAND( ))                                   call zzrc( 74 )     

      IF ( EXT_GET_COMMAND_ARGUMENT( COMMAND_ARGUMENT_COUNT()))  call zzrc( 75 )     

      IF (EXT_GET_ENVIRONMENT_VARIABLE( ))                       call zzrc( 76 )     


      END SUBROUTINE

      END




      FUNCTION EXT_COMMAND_ARGUMENT_COUNT()
      USE MOD
      LOGICAL EXT_COMMAND_ARGUMENT_COUNT


         EXT_COMMAND_ARGUMENT_COUNT = .false.
         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 3 ) & 
         then
           EXT_COMMAND_ARGUMENT_COUNT = .true.
           error stop 63 ! Normally never returns if get here
         endif

      END FUNCTION



      FUNCTION EXT_GET_COMMAND()
      USE MOD
      LOGICAL EXT_GET_COMMAND

        EXT_GET_COMMAND = .false.
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          EXT_GET_COMMAND = .true.
          error stop 64  ! Normally never returns if get here
        endif

      END FUNCTION


      FUNCTION EXT_GET_COMMAND_ARGUMENT(Num)
      USE MOD
      LOGICAL EXT_GET_COMMAND_ARGUMENT
      INTEGER Num

        EXT_GET_COMMAND_ARGUMENT = .false.
        CmdCount = Num
        DO i  = 0, CmdCount
          NUMBER = i
          call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
          call MyGetArg(CmdLine, NUMBER, Argument)
 
          if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
               (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (STATUS      .ne. 0) )                       &
          then
            EXT_GET_COMMAND_ARGUMENT = .true.
            error stop 65 ! Normally never returns if get here
          endif
        END DO


      END FUNCTION


      FUNCTION EXT_GET_ENVIRONMENT_VARIABLE()
      USE MOD
      LOGICAL EXT_GET_ENVIRONMENT_VARIABLE
      INTEGER Num

        EXT_GET_ENVIRONMENT_VARIABLE = .false.
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          EXT_GET_ENVIRONMENT_VARIABLE = .true.
          error stop 66 ! Normally never returns if get here
        endif



      END FUNCTION




      INCLUDE 'cmdline.include'

