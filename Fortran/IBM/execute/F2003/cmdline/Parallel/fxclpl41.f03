! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                :  Call command line intrinsic routines through internal sub and func
!*                             :  which are invoked within section contruct with actual args as
!*                             :  components of derived types
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp41

      IMPLICIT NONE

      integer i, j, k
      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine

      TYPE DerT
        character(2049)  :: COMMAND
        integer          :: LENGTH
        integer          :: STATUS
        integer          :: NUMBER
        character(2047)  :: VALUE
        integer          :: ARGCOUNT
      END TYPE

      TYPE(DerT)  D

      CmdLine = 'fxclpl41 qwerioqweriuheq --rweqkjqwhwef-wefkjfw er'
      NAME = 'CmdLine   '
      TRIM_NAME = .true.


   !$OMP PARALLEL  PRIVATE(D)  SHARED(CmdLine, NAME, TRIM_NAME)
   !$OMP SECTIONS

   !$OMP SECTION
      do i = 1, 4
          call sub
      end do
   !$OMP END SECTIONS
   !$OMP END PARALLEL


   !$OMP PARALLEL  PRIVATE(D)  SHARED(CmdLine, NAME, TRIM_NAME)
   !$OMP SECTIONS
   !$OMP SECTION
      do j = 1, 4
          k= FUN()
      end do
   !$OMP END SECTIONS
   !$OMP END PARALLEL


     CONTAINS

     SUBROUTINE SUB

       integer                   :: CmdCount, i, k
       character(2047)           :: Argument


       CmdCount = COMMAND_ARGUMENT_COUNT()
       if ( CmdCount .ne. 3 ) &
       then
         error stop 63
       endif

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

     END SUBROUTINE



     FUNCTION FUN()

       INTEGER            :: FUN
       integer            :: CmdCount, i, k
       character(2047)    :: Argument


       call GET_COMMAND(D%COMMAND, D%LENGTH, D%STATUS)

       if ( (TRIM(D%COMMAND) .ne. TRIM(CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
            (D%STATUS .ne. 0) )                        &
       then
         error stop 64
       endif


       call GET_ENVIRONMENT_VARIABLE(NAME, D%VALUE, D%LENGTH, D%STATUS, TRIM_NAME)
       if ( (TRIM(D%VALUE) .ne. TRIM(CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (D%STATUS .ne. 0))                       &
       then
         error stop 66
       endif

       FUN = 0

     END FUNCTION


      END

      INCLUDE 'cmdline.include'
