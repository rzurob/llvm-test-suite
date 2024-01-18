! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat35 \#\#\#\#\#\# \!\!\!\!\!"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat35
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat35.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing pointees 
!*                             : of derivrd types from defferent modules
!*                             : as arguments
!*                        
!*                    
!234567890123456789012345678901234567890123456789012345678901234567890


      module m2  

        type COMMAND
          sequence
          character(2049)  :: C
        end type COMMAND

      end module m2

      module m3

        type  LENGTH
          sequence
          integer  :: C 
        end type  LENGTH

      end module m3

      module m4

        type  STATUS
          sequence
          integer  :: C 
        end type  STATUS

     end module m4

      module m5

        type NUMBER
          sequence
          integer  :: C 
        end type NUMBER

      end module m5

      module m6

        type VALUE
          sequence
          character(2047)  :: C 
        end type VALUE

      end module m6

      module m7

        type NAME
          sequence
          character(513)  :: C 
        end type NAME

      end module m7

      module m8

        type TRIM_NAME
          sequence
          logical  :: C 
        end type TRIM_NAME

      end module m8

      module m9

        type  ARGCOUNT
          sequence
          integer  :: C 
        end type  ARGCOUNT

      end module m9




      PROGRAM fxclat35


      use m2
      use m3
      use m4
      use m5
      use m6
      use m7
      use m8
      use m9


      IMPLICIT NONE



      type(COMMAND)  ::  COMMANDee, COMMAND
      type(LENGTH)   ::  LENGTHee, LENGTH
      type(STATUS)   ::  STATUSee, STATUS
      type(NUMBER)   ::  NUMBERee, NUMBER
      type(VALUE)    ::  VALUEee, VALUE
      type(NAME)     ::  NAMEee, NAME
      type(TRIM_NAME)::  TRIM_NAMEee, TRIM_NAME
      type(ARGCOUNT) ::  ARGCOUNTee, ARGCOUNT

      character(2049)              :: CmdLine = 'fxclat35 \\#\\#\\#\\#\\#\\# \\!\\!\\!\\!\\!'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      pointer ( PCOMMAND,  COMMAND)
      pointer (PLENGTH,    LENGTH)
      pointer (PSTATUS,    STATUS)
      pointer (PNUMBER,    NUMBER)
      pointer (PVALUE,     VALUE)
      pointer (PNAME,      NAME)
      pointer (PTRIM_NAME, TRIM_NAME)
      pointer (PARGCOUNT,  ARGCOUNT)
      

      PCOMMAND    = LOC(COMMANDee)
      PLENGTH     = LOC(LENGTHee)
      PSTATUS     = LOC(STATUSee)
      PNUMBER     = LOC(NUMBERee)
      PVALUE      = LOC(VALUEee)
      PNAME       = LOC(NAMEee)
      PTRIM_NAME  = LOC(TRIM_NAMEee)
      PARGCOUNT   = LOC(ARGCOUNTee)


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 2 ) & 
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND%C, LENGTH%C, STATUS%C)

      if ( (TRIM(COMMAND%C) .ne. TRIM(CmdLine))  .or. &
           (LENGTH%C .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS%C .ne. 0) )                        &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount
       
        NUMBER%C = i
        call GET_COMMAND_ARGUMENT(NUMBER%C, VALUE%C, LENGTH%C, STATUS%C)
        call MyGetArg(CmdLine, NUMBER%C, Argument)
        if ( (TRIM(VALUE%C) .ne. TRIM(Argument))       .or. &
             (LENGTH%C      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS%C      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      NAME%C = 'CmdLine     '
      TRIM_NAME%C = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME%C, VALUE%C, LENGTH%C, STATUS%C, TRIM_NAME%C)
      if ( (TRIM(VALUE%C) .ne. TRIM(CmdLine))  .or. &
           (LENGTH%C .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS%C .ne. 0))                       &
      then
        error stop 66
      endif


      END 
 
      INCLUDE 'cmdline.include'



  
 
