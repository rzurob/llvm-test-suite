! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat34 \! \! \! \!"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat34
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat34.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing array elements   
!*                             : as actual arguments
!*
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

      module modtype

        type rec
          character(2049)  :: COMMAND(8)    !1
          integer      	   :: LENGTH(8)     !2
          integer          :: STATUS(8)     !3
          integer          :: NUMBER(8)     !4
          character(2047)  :: VALUE(8)      !5
          character(513)   :: NAME(8)       !6
          logical          :: TRIM_NAME(8)  !7
          integer          :: ARGCOUNT(8)   !8
        end type
         
      end module modtype

 
      PROGRAM fxclat34
      
      USE modtype
      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat34 \\! \\! \\! \\!'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(rec), allocatable ::  cmd(:)

      allocate(cmd(3))

      cmd(2)%NAME(6) = 'CmdLine     '
      cmd(2)%TRIM_NAME(8)  =  .true.


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) & 
      then
        error stop 63
      endif

      call GET_COMMAND(cmd(2)%COMMAND(1), cmd(2)%LENGTH(2), cmd(2)%STATUS(3))


      if ( (TRIM(cmd(2)%COMMAND(1)) .ne. TRIM(CmdLine))     .or. &
           (cmd(2)%LENGTH(2) .ne. LEN(TRIM(CmdLine)))       .or. &
           (cmd(2)%STATUS(3) .ne. 0) )                           &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount
       
        cmd(2)%NUMBER(4) = i
        call GET_COMMAND_ARGUMENT(cmd(2)%NUMBER(4), cmd(2)%VALUE(5), cmd(2)%LENGTH(2), cmd(2)%STATUS(3))
        call MyGetArg(CmdLine, cmd(2)%NUMBER(4), Argument)

        if ( (TRIM(cmd(2)%VALUE(5)) .ne. TRIM(Argument))          .or. &
             (cmd(2)%LENGTH(2)         .ne. LEN(TRIM(Argument)))  .or. &
             (cmd(2)%STATUS(3)      .ne. 0) )                          &
        then
          error stop 65
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE(cmd(2)%NAME(6), cmd(2)%VALUE(5), cmd(2)%LENGTH(2), cmd(2)%STATUS(3), cmd(2)%TRIM_NAME(8))
      if ( (TRIM(cmd(2)%VALUE(5)) .ne. TRIM(CmdLine))     .or. &
           (cmd(2)%LENGTH(2) .ne. LEN(TRIM(CmdLine)))     .or. &
           (cmd(2)%STATUS(3) .ne. 0))                          &
      then
        error stop 66
      endif


    
      deallocate(cmd)

      END 
 
      INCLUDE 'cmdline.include'

  
    
