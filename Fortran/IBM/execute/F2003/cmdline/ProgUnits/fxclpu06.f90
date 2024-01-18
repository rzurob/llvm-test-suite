! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu06 1 a"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu06
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu06.f
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
!*  DESCRIPTION                : Call command line procedures through external subroutine
!*                             : which is used for the defined assignment(=) 
!*                             : with actual arguments defined in derived type in module 
!*                             : 
!*                             :
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD
 
      TYPE DT

      character(2049)   :: COMMAND
      integer           :: LENGTH
      integer           :: STATUS
      integer           :: NUMBER
      character(2047)   :: VALUE
      character(513)    :: NAME
      logical           :: TRIM_NAME
      integer           :: ARGCOUNT

      END TYPE DT

      END MODULE


      PROGRAM fxclpu06
      USE MOD
      IMPLICIT NONE

      INTERFACE ASSIGNMENT ( = )
        SUBROUTINE EXT_SUB( A,B)
          USE MOD
          TYPE(DT), INTENT(out)  :: A
          TYPE(DT), INTENT(in )  :: B
        END SUBROUTINE
      END INTERFACE

      TYPE(DT) :: A 
      TYPE(DT) :: B 


      A = B
      B = A        

      END


      SUBROUTINE EXT_SUB( B,A)
      USE MOD

      TYPE(DT), INTENT(out)  :: B
      TYPE(DT), INTENT(in )  :: A

      
      character(2049)              :: CmdLine = 'fxclpu06 1 a'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 2 ) & 
      then
        error stop 63
      endif

      call GET_COMMAND(B%COMMAND, B%LENGTH, B%STATUS)
      if ( (TRIM(B%COMMAND) .ne. TRIM(CmdLine))  .or. &
           (B%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (B%STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


      DO i  = 0, CmdCount
       
        B%NUMBER = i
        call GET_COMMAND_ARGUMENT(B%NUMBER, B%VALUE, B%LENGTH, B%STATUS)
        call MyGetArg(CmdLine, B%NUMBER, Argument)
        if ( (TRIM(B%VALUE) .ne. TRIM(Argument))       .or. &
             (B%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (B%STATUS       .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO


      B%NAME = 'CmdLine     '
      B%TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(B%NAME, B%VALUE, B%LENGTH, B%STATUS, B%TRIM_NAME)
      if ( (TRIM(B%VALUE) .ne. TRIM(CmdLine))  .or. &
           (B%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (B%STATUS .ne. 0))                       &
      then
        error stop 66
      endif



      END SUBROUTINE

 
      INCLUDE 'cmdline.include'


