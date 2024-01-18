! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu37 1 - 2 = -3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu37
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu37.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 1, 2003
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
!*  DESCRIPTION                : Invoke command line procedures within external functions 
!*                             : through interface with the derived type of return value 
!*                             : used by these intrinsic routines
!*                             : (Check if the intrinsic affect other storage)
!* 
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD

      TYPE CMD

        character(2049)    :: COMMAND /'??????????????????????'/
        integer            :: LENGTH  /123/
        character(2049)    :: CmdLine /'fxclpu37 1 - 2 = -3'/
        integer            :: CmdCount /5/

        integer            :: STATUS  /321/
        integer            :: NUMBER  /111/
        character(2047)    :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


        character(513)     :: NAME      /'CmdLine  '/
        logical            :: TRIM_NAME /.true./

        character(2047)    :: Argument
 
      END TYPE

      TYPE(CMD), SAVE      :: Junk

      END MODULE


      PROGRAM fxclpu37
      USE MOD

      INTEGER i
      
      INTERFACE 
        FUNCTION F_GET_COMMAND() 
        USE MOD
          TYPE(CMD) F_GET_COMMAND
        END FUNCTION

        FUNCTION F_GET_COMMAND_ARGUMENT()
        USE MOD
          TYPE(CMD) F_GET_COMMAND_ARGUMENT
        END FUNCTION
  
        FUNCTION F_GET_ENVIRONMENT_VARIABLE()  
        USE MOD
          TYPE(CMD) F_GET_ENVIRONMENT_VARIABLE
        END FUNCTION     
      END INTERFACE


      INTERFACE 
        FUNCTION F_COMMAND_ARGUMENT_COUNT(CmdCount)
          INTEGER CmdCount
          INTEGER F_COMMAND_ARGUMENT_COUNT
        END FUNCTION
      END INTERFACE


      IF ( F_COMMAND_ARGUMENT_COUNT(Junk.CmdCount) .ne. Junk.CmdCount) ERROR STOP 63

      Junk = F_GET_COMMAND()

      Junk = F_GET_COMMAND_ARGUMENT()

      Junk = F_GET_ENVIRONMENT_VARIABLE()
  
      IF (Junk%NAME .ne. 'CmdLine  ' )              ERROR STOP 67
      IF (.not.Junk%TRIM_NAME )                     ERROR STOP 68
      IF (Junk%CmdCount .ne. 5)                     ERROR STOP 69
      IF (Junk%CmdLine .ne. 'fxclpu37 1 - 2 = -3')  ERROR STOP 70


      END


      FUNCTION F_COMMAND_ARGUMENT_COUNT(CmdCount)
      USE MOD
      IMPLICIT NONE

      INTEGER F_COMMAND_ARGUMENT_COUNT
      INTEGER CmdCount

      F_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT()) & 
      then
        error stop 63
      endif

      END FUNCTION


      FUNCTION F_GET_COMMAND()
      USE MOD
      IMPLICIT  NONE

      TYPE(CMD) F_GET_COMMAND

      call GET_COMMAND(               &
               Junk%COMMAND, &
               Junk%LENGTH,  &
               Junk%STATUS)

      if ( (TRIM(Junk%COMMAND) .ne. TRIM(Junk%CmdLine))  .or. &
           (Junk%LENGTH .ne. LEN(TRIM(Junk%CmdLine)))    .or. &
           (Junk%STATUS .ne. 0) )                                      &
      then
        error stop 64
      endif

      F_GET_COMMAND = Junk

      END FUNCTION

 
      FUNCTION F_GET_COMMAND_ARGUMENT()
      USE MOD
      IMPLICIT NONE

      TYPE(CMD) F_GET_COMMAND_ARGUMENT
      INTEGER i

      DO i  = 0, Junk%CmdCount
       
        Junk%NUMBER = i
        call GET_COMMAND_ARGUMENT(                &
                Junk%NUMBER,    &
                Junk%VALUE,     &
                Junk%LENGTH,    &
                Junk%STATUS)

        call MyGetArg(Junk%CmdLine, &
                    Junk%NUMBER,  &
                    Junk% Argument)
        if ( (TRIM(Junk%VALUE) .ne. TRIM(Junk%Argument))       .or. &
             (Junk%LENGTH       .ne. LEN(trim(Junk%Argument))) .or. &
             (Junk%STATUS       .ne. 0) )                           &
        then
          error stop 65
        endif

      END DO

      F_GET_COMMAND_ARGUMENT = Junk

      END FUNCTION


      FUNCTION F_GET_ENVIRONMENT_VARIABLE()
      USE MOD
      IMPLICIT  NONE

      TYPE(CMD) F_GET_ENVIRONMENT_VARIABLE

      call GET_ENVIRONMENT_VARIABLE(      &
            Junk%NAME, &
            Junk%VALUE,  &
            Junk%LENGTH, &
            Junk%STATUS, &
            Junk%TRIM_NAME)

      if ( (TRIM(Junk%VALUE) .ne. TRIM(Junk%CmdLine))  .or. &
           (Junk%LENGTH .ne. LEN(TRIM(Junk%CmdLine)))  .or. &
           (Junk%STATUS .ne. 0))                            &
      then
        error stop 66
      endif

      F_GET_ENVIRONMENT_VARIABLE = Junk

      END FUNCTION


      INCLUDE 'cmdline.include'


  
