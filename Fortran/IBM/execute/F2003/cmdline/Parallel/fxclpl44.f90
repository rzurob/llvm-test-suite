! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl44 \A \B \C \D"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl44
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl44.f
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
!*  DESCRIPTION                :  Call command line intrinsic routines within a main through multiple 
!*                             :  single construct with arguments specified as components od derived types
!*                             :  
!*             
!*                           
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp44
      
      IMPLICIT NONE

    
      TYPE DerC
        character(513)   :: NAME  
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine 
      END TYPE

      TYPE DerT 
        character(2049)  :: COMMAND
        integer          :: LENGTH     
        integer          :: STATUS  
        integer          :: NUMBER 
        character(2047)  :: VALUE  
        integer          :: ARGCOUNT
      END TYPE

      TYPE(DerT)  D
      TYPE(DerC)  C

      integer                   :: CmdCount
      integer                   :: i, j, k
      character(2047)           :: Argument

      C%CmdLine = 'fxclpl44 \\A \\B \\C \\D'
      C%NAME = 'CmdLine   '
      C%TRIM_NAME = .true.


   !$OMP PARALLEL             &
   !$OMP DEFAULT(PRIVATE)     &
   !$OMP FIRSTPRIVATE(C) 

         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 4 ) & 
         then
           error stop 63
         endif

   !$OMP SINGLE
        DO i  = 0, CmdCount
          D%NUMBER = i
          call GET_COMMAND_ARGUMENT(D%NUMBER, D%VALUE, D%LENGTH, D%STATUS)
          call MyGetArg(C%CmdLine, D%NUMBER, Argument)
          if ( (TRIM(D%VALUE) .ne. TRIM(Argument))       .or. &
               (D%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (D%STATUS      .ne. 0) )                       &
          then
            error stop 65
          endif
        END DO
   !$OMP END SINGLE


   !$OMP SINGLE
       call GET_COMMAND(D%COMMAND, D%LENGTH, D%STATUS)
       if ( (TRIM(D%COMMAND) .ne. TRIM(C%CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(C%CmdLine)))    .or. &
            (D%STATUS .ne. 0) )                          &
       then
         error stop 64
       endif
   !$OMP END SINGLE


   !$OMP SINGLE
       call GET_ENVIRONMENT_VARIABLE(C%NAME, D%VALUE, D%LENGTH, D%STATUS, C%TRIM_NAME)
       if ( (TRIM(D%VALUE) .ne. TRIM(C%CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(C%CmdLine)))  .or. &
            (D%STATUS .ne. 0))                         &
       then
         error stop 66
       endif
   !$OMP END SINGLE

   !$OMP END PARALLEL   


      END 
 
      INCLUDE 'cmdline.include'

