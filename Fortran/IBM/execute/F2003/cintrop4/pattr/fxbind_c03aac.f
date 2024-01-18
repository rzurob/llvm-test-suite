! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c03aac  cxbind_c03aac
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c03aac.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran programs interoperate with C functions
!*                                through a Fortran procedure interface that uses
!*                                the BIND specification .
!*            
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           character*1,logical*1.
!*           The datatype logical*2, logical*4,logical*8 and 
!*           byte is not supported.
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in FORTRAN, Fortran calls C functions.
!*
!*  ALGORITHM :  
!*          1. Declare the interop functions in Fortran program.
!*          ( Create a procedural interface that corresponds to the C prototype
!*          and bind the interface to the C function using the BIND(C) specifier). 
!*          2. Initialize the variable which will be the  actual arguments of
!*             the interop functions. 
!*          3. Fortran  program call C function.The argument is  altered
!*             during execution of the C Function.
!*          4. Assertion: Check the modified auguments and return value  
!*             in Fortran to verify it is correct.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxbind_c03aac
  use assertmod
  implicit none

  interface
     function fun_char_ref(x,y) BIND(C)
       character*1  :: x,y
       character*1  :: fun_char_ref
     end function fun_char_ref

     function fun_char_val(x,y) BIND(C)
       character*1,value  :: x,y
       character*1  :: fun_char_val
     end function fun_char_val

     function exfun_log_ref1(l1, l) BIND(C)
       logical*1 l1
       logical*1  l
       logical*1 exfun_log_ref1
     end function exfun_log_ref1

     function exfun_log_val1(l1, l) BIND(C)
       logical*1 ,value :: l1
       logical*1 ,value ::l
       logical*1 exfun_log_val1
     end function exfun_log_val1

     function TryRetLog (r)  BIND(C)
       LOGICAL*1  r, RetLog  
     END function TryRetLog

  end interface

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************
  character*1 ach_ref /'A'/, bch_ref /'B'/, res_ch_ref
  character*1 ach_val /'A'/, bch_val /'B'/, res_ch_val
  logical*1 al_ref1 /.false./, res_log_ref1
  logical*1 al_val1 /.false./, res_log_val1
  logical*1 l /.true./
  logical*1 r, s, RetLog  

  !**********************************************************
  !        Calling C from Fortran with character data type
  !                and check the Results
  !**********************************************************

  ! Test 1 : call by reference
  ! A dummy argument without the VALUE attribute correspond 
  ! to a formal parameter  of the prototype in C program 
  ! that is of a pointer type.

  res_ch_ref = fun_char_ref(ach_ref,bch_ref)
  if (ach_ref /= 'C' ) error stop 20
  if (bch_ref /= 'D' ) error stop 21
  if(res_ch_ref .ne. 'E')then
     error stop 22
  endif

  ! Test 2 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is 
  ! not of a pointer type.

  res_ch_val = fun_char_val(ach_val,bch_val)
  if (ach_val /= 'A' ) error stop 20
  if (bch_val /= 'B' ) error stop 21
  if(res_ch_val .ne. 'E')then
     error stop 22
  endif

  !**********************************************************
  !        Calling C from Fortran with logical data type
  !                and check the Results
  !**********************************************************
  ! Test 3 : call by reference
  ! A dummy argument without the VALUE attribute correspond 
  ! to a formal parameter  of the prototype in C program 
  ! that is of a pointer type.

  res_log_ref1 = exfun_log_ref1(al_ref1, l)

  if(al_ref1 .neqv. .true.)then
     error stop 29
  endif

  if(res_log_ref1 .neqv. .true.)then
     error stop 30
  endif

  ! Test 4 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is 
  ! not of a pointer type.

  res_log_val1 = exfun_log_val1(al_val1, l)

  if(al_val1 .neqv. .false.)then
     error stop 31
  endif

  if(res_log_val1 .neqv. .true.)then
     error stop 32
  endif

  ! Test Fortran and C with a LOGICAL function return value
  r = .FALSE.  
  s = .TRUE. .AND. RetLog(r)  
  call assert(s,'Hello, the result is not correct!',40)

end program fxbind_c03aac
