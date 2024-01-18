! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf15 /-/-/-/-/-/ -/-/-/-/ _+_+_+_"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf15
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf15.f
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
!*  DESCRIPTION                : Call command line intrinsic routines by passing  expressions 
!*                             : as actual intent(in) arguments 
!*                             : 
!*                             : 
!*           
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxcllf15

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxcllf15 /-/-/-/-/-/ -/-/-/-/ _+_+_+_'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT
      character(513)              :: NAME  
      logical                     :: TRIM_NAME 

      DATA NAME          /'CmdLine   '/
      DATA TRIM_NAME     /.true./



      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) & 
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND(23:2048), LENGTH, STATUS)
      call GET_COMMAND()

      if ( (TRIM(COMMAND(23:2048)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (STATUS .ne. 0) )                                &
      then
        error stop 64
      endif


       
       DO i  = 0, CmdCount
          NUMBER = i
          call GET_COMMAND_ARGUMENT(0 + NUMBER*10**0 - 0, VALUE =VALUE(3:333), LENGTH=LENGTH, STATUS=STATUS)
          call MyGetArg(CmdLine, NUMBER, Argument)
 
          if ( (TRIM(VALUE(3:333)) .ne. TRIM(Argument))       .or. &
               (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (STATUS      .ne. 0) )                       &
          then
            error stop 65
          endif
       END DO


      
        call GET_ENVIRONMENT_VARIABLE(NAME// '   ' // ' ', VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif




      END 
 
      INCLUDE 'cmdline.include'


