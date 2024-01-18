! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat20 1 a 2 b 3"
! %COMPOPTS:  -qfree=f90 -qintsize=8
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat20
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat20.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing string sections of
!*                             : components of derived type with various optional arguments 
!*                             : as actual arguments
!*                             : 
!*              
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        type dertype
          sequence
          character(2049)             :: COMMAND
          integer      	              :: LENGTH
          character(4099)             :: STR = '1234567890'
          integer                     :: STATUS
          integer                     :: NUMBER
          character(2047)             :: VALUE
          INTEGER                     :: ARR(10) 
          integer                     :: ARGCOUNT
        end type dertype 

      end module modtype


      PROGRAM fxclat20

      use modtype

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat20 1 a 2 b 3'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(dertype) cmd

      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE  


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 5 ) & 
      then
        call zzrcy4(63)
      endif

      call GET_COMMAND(cmd%COMMAND(33:532), cmd%LENGTH, cmd%STATUS)
      call GET_COMMAND()
      if ( (TRIM(cmd%COMMAND(33:532)) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%STATUS .ne. 0) )                        &
      then
        call zzrcy4(64)
      endif

      call GET_COMMAND(COMMAND(1:101), LENGTH)
      if ( (TRIM(COMMAND(1:101)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))) )              &
      then
        call zzrcy4(65)
      endif

      call GET_COMMAND(COMMAND(1001:2049))
      if ( TRIM(COMMAND(1001:2049)) .ne. TRIM(CmdLine))  &
      then
        call zzrcy4(66)
      endif


      DO i  = 0, CmdCount
       
        cmd%NUMBER = i
        call MyGetArg(CmdLine, cmd%NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE(1023:2046), cmd%LENGTH, cmd%STATUS)
        call GET_COMMAND_ARGUMENT(cmd%NUMBER)

        if ( (TRIM(cmd%VALUE(1023:2046)) .ne. TRIM(Argument))       .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          call zzrcy4(67)
        endif

        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE(513:688), cmd%LENGTH)
        if ( (TRIM(cmd%VALUE(513:688)) .ne. TRIM(Argument))      .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument))))              &
        then
          call zzrcy4(68)
        endif

        call GET_COMMAND_ARGUMENT(cmd%NUMBER, VALUE =VALUE(11:511), STATUS=STATUS)
        if ( (TRIM(VALUE(11:511)) .ne. TRIM(Argument))       .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          call zzrcy4(69)
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', cmd%VALUE(1013:2039), cmd%LENGTH, cmd%STATUS, .true.)
      if ( (TRIM(cmd%VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (cmd%STATUS .ne. 0))                       &
      then
        call zzrcy4(70)
      endif

      call GET_ENVIRONMENT_VARIABLE('CmdLine', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false.)
      if ( (cmd%LENGTH .ne.LENGTH)  .or. &
           (cmd%STATUS .ne. STATUS))     &
      then
        call zzrcy4(71)
      endif


      END 
 

      INCLUDE 'cmdline.include'


      ! Currently ZZRC only support default int size !
      SUBROUTINE ZZRCY4(RC)
        integer RC
        integer( kind=4) :: RC4

        RC4= RC
        call zzrc( RC4 )

      END SUBROUTINE


  
        

