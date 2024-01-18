! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl45 1 a 2
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl45
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl45.f
!*
!*  DATE                       : Oct 1, 2003
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
!*  DESCRIPTION                :  Call command line intrinsic routines  within parallel do construct
!*                             :  with different data types in shared clause
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp45

      IMPLICIT NONE

      TYPE DerT
        character(2049)  :: COMMAND
        integer          :: LENGTH
        integer          :: STATUS
        integer          :: NUMBER
        character(2047)  :: VALUE
        integer          :: ARGCOUNT
      END TYPE

      TYPE(DerT)  D

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine
      integer                   :: CmdCount

      integer                   :: i, j, k
      character(2047)           :: Argument

      CmdLine = 'fxclpl45 1 a 2'
      NAME = 'CmdLine   '
      TRIM_NAME = .true.


         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 3 ) &
         then
           error stop 63
         endif



   !$OMP PARALLEL          &
   !$OMP DEFAULT(PRIVATE)  &
   !$OMP SHARED(CmdCount)  &
   !$OMP SHARED(CmdLine)
       call GET_COMMAND(D%COMMAND, D%LENGTH, D%STATUS)
       if ( (TRIM(D%COMMAND) .ne. TRIM(CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
            (D%STATUS .ne. 0) )                        &
       then
         error stop 64
       endif
   !$OMP END PARALLEL



   !$OMP PARALLEL   DO     &
   !$OMP DEFAULT(PRIVATE)  &
   !$OMP SHARED(CmdCount)  &
   !$OMP SHARED(CmdLine)
        DO i  = 0, CmdCount
          D%NUMBER = i
          call GET_COMMAND_ARGUMENT(D%NUMBER, D%VALUE, D%LENGTH, D%STATUS)
          call MyGetArg(CmdLine, D%NUMBER, Argument)
          if ( (TRIM(D%VALUE) .ne. TRIM(Argument))       .or. &
               (D%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (D%STATUS      .ne. 0) )                       &
          then
            error stop 65
          endif
        END DO
   !$OMP END PARALLEL   DO



   !$OMP PARALLEL          &
   !$OMP DEFAULT(PRIVATE)  &
   !$OMP SHARED(CmdCount)  &
   !$OMP SHARED(CmdLine)   &
   !$OMP SHARED(NAME)      &
   !$OMP SHARED(TRIM_NAME)
       call GET_ENVIRONMENT_VARIABLE(NAME, D%VALUE, D%LENGTH, D%STATUS, TRIM_NAME)
       if ( (TRIM(D%VALUE) .ne. TRIM(CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (D%STATUS .ne. 0))                       &
       then
         error stop 66
       endif
   !$OMP END PARALLEL



      END

      INCLUDE 'cmdline.include'

