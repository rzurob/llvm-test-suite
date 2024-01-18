! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu38 +1 + -2 = --3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu38
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu38.f
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
!*  DESCRIPTION                : Invoke command line procedures within external recursive functions 
!*                             : through interface with the derived type of return value 
!*                             : used by these intrinsic routines
!*                             : (Check if the intrinsic routines affect other storage)
!* 
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD

      TYPE CMD

        character(2049)  :: COMMAND /'??????????????????????'/
        integer          :: LENGTH  /123/
        character(2049)  :: CmdLine /'fxclpu38 +1 + -2 = --3'/
        integer          :: CmdCount /5/

        integer          :: STATUS  /321/
        integer          :: NUMBER  /111/
        character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


        character(513)   :: NAME      /'CmdLine  '/
        logical          :: TRIM_NAME /.true./

        character(2047)  :: Argument
 
      END TYPE

      TYPE(CMD), SAVE    :: Junk


      END MODULE


      PROGRAM fxclpu38
      USE MOD


      INTEGER i
      
      INTERFACE 
        RECURSIVE FUNCTION F_GET_COMMAND(Num) 
          USE MOD
          TYPE(CMD) F_GET_COMMAND
          INTEGER   Num
        END FUNCTION

        RECURSIVE FUNCTION F_GET_COMMAND_ARGUMENT(Num)
          USE MOD
          TYPE(CMD) F_GET_COMMAND_ARGUMENT
          INTEGER   Num
        END FUNCTION
  
        RECURSIVE FUNCTION F_GET_ENVIRONMENT_VARIABLE(Num)  
          USE MOD
          TYPE(CMD) F_GET_ENVIRONMENT_VARIABLE
          INTEGER   Num
        END FUNCTION     
      END INTERFACE


      INTERFACE 
        RECURSIVE FUNCTION F_COMMAND_ARGUMENT_COUNT(Num, CmdCount)
          INTEGER CmdCount
          INTEGER F_COMMAND_ARGUMENT_COUNT
          INTEGER Num
        END FUNCTION
      END INTERFACE


      IF ( F_COMMAND_ARGUMENT_COUNT(5, Junk.CmdCount) .ne. Junk.CmdCount) ERROR STOP 63

      Junk = F_GET_COMMAND(5)

      Junk = F_GET_COMMAND_ARGUMENT(5)

      Junk = F_GET_ENVIRONMENT_VARIABLE(5)
  
      IF (Junk%NAME .ne. 'CmdLine  ' )              ERROR STOP 67
      IF (.not.Junk%TRIM_NAME )                     ERROR STOP 68
      IF (Junk%CmdCount .ne. 5)                     ERROR STOP 69
      IF (Junk%CmdLine .ne. 'fxclpu38 +1 + -2 = --3')  ERROR STOP 70


      END


      RECURSIVE FUNCTION F_COMMAND_ARGUMENT_COUNT(Num, CmdCount)
      USE MOD
      IMPLICIT NONE

      INTEGER F_COMMAND_ARGUMENT_COUNT
      INTEGER CmdCount, Num


      IF ( Num .gt. 1) THEN
         F_COMMAND_ARGUMENT_COUNT = F_COMMAND_ARGUMENT_COUNT(Num - 1, CmdCount)
      ELSE

        F_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()

        if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT()) & 
        then
          error stop 63
        endif

      END IF

      END FUNCTION


      RECURSIVE FUNCTION F_GET_COMMAND(Num)
      USE MOD
      IMPLICIT  NONE

      TYPE(CMD) F_GET_COMMAND
      INTEGER   Num

  
      IF ( Num .gt. 1) THEN
         F_GET_COMMAND = F_GET_COMMAND(Num - 1)
      ELSE


      call GET_COMMAND(               &
               Junk%COMMAND, &
               Junk%LENGTH,  &
               Junk%STATUS)

      if ( (TRIM(Junk%COMMAND) .ne. TRIM(Junk%CmdLine))  .or. &
           (Junk%LENGTH .ne. LEN(TRIM(Junk%CmdLine)))   .or. &
           (Junk%STATUS .ne. 0) )                             &
      then
        error stop 64
      endif

      F_GET_COMMAND = Junk

      END IF

      END FUNCTION

 
      RECURSIVE FUNCTION F_GET_COMMAND_ARGUMENT(Num)
      USE MOD
      IMPLICIT NONE

      TYPE(CMD) F_GET_COMMAND_ARGUMENT
      INTEGER   i
      INTEGER   Num

      IF ( Num .gt. 1) THEN
        F_GET_COMMAND_ARGUMENT = F_GET_COMMAND_ARGUMENT(Num - 1)
      ELSE

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
             (Junk%LENGTH      .ne. LEN(TRIM(Junk%Argument)))  .or. &
             (Junk%STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      F_GET_COMMAND_ARGUMENT = Junk

      END IF

      END FUNCTION


      RECURSIVE FUNCTION F_GET_ENVIRONMENT_VARIABLE(Num)
      USE MOD
      IMPLICIT  NONE

      TYPE(CMD) F_GET_ENVIRONMENT_VARIABLE
      INTEGER   Num

      IF ( Num .gt. 1) THEN
        F_GET_ENVIRONMENT_VARIABLE = F_GET_ENVIRONMENT_VARIABLE(Num - 1)
      ELSE

      call GET_ENVIRONMENT_VARIABLE(      &
            Junk%NAME, &
            Junk%VALUE,  &
            Junk%LENGTH, &
            Junk%STATUS, &
            Junk%TRIM_NAME)

      if ( (TRIM(Junk%VALUE) .ne. TRIM(Junk%CmdLine))  .or. &
           (Junk%LENGTH .ne. LEN(TRIM(Junk%CmdLine)))  .or. &
           (Junk%STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END IF


      F_GET_ENVIRONMENT_VARIABLE = Junk

      END FUNCTION


      INCLUDE 'cmdline.include'


  

