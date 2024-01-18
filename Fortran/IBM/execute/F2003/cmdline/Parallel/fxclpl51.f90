! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl51 \!\[ABCDE\] ---------\@ 1234"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl51
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl51.f
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
!*  DESCRIPTION                : Call command line intrinsic routines  within an elemental function
!*                             : which is invoked by  forall constructs in parallel region with workshare directive
!*                    
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME  
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine 
          
        COMMON /sargs/CmdLine, NAME, TRIM_NAME

      END MODULE 


      BLOCK DATA 

        character(513)   :: NAME  
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine 
          
        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        DATA CmdLine /"fxclpl51 \\!\\[ABCDE\\] ---------\\@ 1234"/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxclpl51

      USE MOD
      IMPLICIT NONE


      INTERFACE 

        PURE LOGICAL FUNCTION SF_GET_CMD(NoUse)
        INTEGER, intent(in)::NoUse
        END FUNCTION

        PURE LOGICAL FUNCTION SF_GET_CMD_ARG(iCount)
          INTEGER, intent(in)::iCOUNT
        END FUNCTION

        PURE LOGICAL FUNCTION SF_GET_ENV_VAR(NoUse)
        INTEGER, intent(in)::NoUse
        END FUNCTION

      END INTERFACE

 
      INTEGER  CMD_ARG_COUNT
      LOGICAL  GET_CMD
      LOGICAL  GET_CMD_ARG 
      LOGICAL  GET_ENV_VAR
 

      LOGICAL  NumOfExec(10), LJunk(10)
      INTEGER  Junk(10), I, NoUse


      GET_CMD(NoUse)      =  SF_GET_CMD(NoUse)
      GET_CMD_ARG()       =  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
      GET_ENV_VAR(NoUse)  =  SF_GET_ENV_VAR(NoUse) 



     NumOfExec = .true.

    !$OMP  PARALLEL  

    !$OMP WORKSHARE
      FORALL (I = 1:10, NumOfExec(I) .eqv. .true.) 
        Junk(I) = COMMAND_ARGUMENT_COUNT()
      END FORALL
    !$OMP END WORKSHARE

        if ( ANY(Junk .ne. 3 ) ) & 
        then
          error stop 73
        endif


    !$OMP WORKSHARE
      FORALL (I = 1:10, NumOfExec(I) .eqv. .true.) 
       LJunk(I) = GET_CMD(NoUse) 
      END FORALL
    !$OMP END WORKSHARE

       if ( ANY(LJunk .eqv. .false. ) ) & 
       then
         error stop 74
       endif

    !$OMP WORKSHARE
      FORALL (I = 1:10, NumOfExec(I) .eqv. .true.) 
       LJunk(I) = GET_CMD_ARG()
      END FORALL
    !$OMP END WORKSHARE

       if ( ANY(LJunk .eqv. .false. ) ) & 
       then
         error stop 75
       endif

    !$OMP WORKSHARE
      FORALL (I = 1:10, NumOfExec(I) .eqv. .true.) 
       LJunk(I) = GET_ENV_VAR(NoUse) 
      END FORALL
    !$OMP END WORKSHARE

       if ( ANY(LJunk .eqv. .false. ) ) & 
       then
         error stop 76
       endif

    !$OMP END PARALLEL  

      END 


      ELEMENTAL FUNCTION SF_GET_CMD(NoUse)

      USE MOD

      LOGICAL SF_GET_CMD
      INTEGER, INTENT(IN) :: NoUse

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_CMD = .true.

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then 
        SF_GET_CMD = .false. 
        ! error stop 64
      endif

      END FUNCTION 

      FUNCTION SF_GET_CMD_ARG(CmdCount)  ! This func is not elemental because of GetArg

      USE MOD

      LOGICAL SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer, INTENT(IN)  :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_CMD_ARG = .true.

      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          SF_GET_CMD_ARG = .false.
         ! error stop 65
        endif

      END DO

     END FUNCTION



      ELEMENTAL FUNCTION SF_GET_ENV_VAR(NoUse) 

      USE MOD

      LOGICAL SF_GET_ENV_VAR
      INTEGER, INTENT(IN) :: NoUse

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_ENV_VAR = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         SF_GET_ENV_VAR = .false.
         ! error stop 66
      endif


      END FUNCTION
 
      INCLUDE 'cmdline.include'

