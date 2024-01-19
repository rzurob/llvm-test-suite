! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing pointers of variuos types
!*                             : with int size =2  as arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclat41

      IMPLICIT NONE

      character(2049), pointer :: PCOMMAND
      integer,         pointer :: PLENGTH
      integer,         pointer :: PSTATUS
      integer,         pointer :: PNUMBER
      character(2049), pointer :: PVALUE
      character(2049), pointer :: PNAME
      logical,         pointer :: PTRIM_NAME
      integer,         pointer :: PARGCOUNT

      character(2049)        ::  CmdLine = 'fxclat41 1 a 2 b 3'
      integer                ::  CmdCount, i
      character(2047)        ::  Argument

      allocate (PCOMMAND, PLENGTH, PSTATUS, PNUMBER, PVALUE, PNAME, PTRIM_NAME, PARGCOUNT)


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 5 ) &
      then
        error stop 63_4
      endif
      call GET_COMMAND(PCOMMAND, PLENGTH, PSTATUS)
      if ( (TRIM(PCOMMAND) .ne. TRIM(CmdLine))  .or. &
           (PLENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (PSTATUS .ne. 0) )                        &
      then
        error stop 64_4
      endif

      DO i  = 0, CmdCount

        PNUMBER = i
        call GET_COMMAND_ARGUMENT(PNUMBER, PVALUE, PLENGTH, PSTATUS)
        call MyGetArg(CmdLine, PNUMBER, Argument)
        if ( (TRIM(PVALUE) .ne. TRIM(Argument))       .or. &
             (PLENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (PSTATUS      .ne. 0) )                       &
        then
          error stop 65_4
        endif

      END DO

      PNAME = 'CmdLine     '
      PTRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(PNAME, PVALUE, PLENGTH, PSTATUS, PTRIM_NAME)
      if ( (TRIM(PVALUE) .ne. TRIM(CmdLine))  .or. &
           (PLENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (PSTATUS .ne. 0))                       &
      then
        error stop 66_4
      endif


      deallocate (PCOMMAND, PLENGTH, PSTATUS, PNUMBER, PVALUE, PNAME, PTRIM_NAME, PARGCOUNT)


      END

      INCLUDE 'cmdline.include'





