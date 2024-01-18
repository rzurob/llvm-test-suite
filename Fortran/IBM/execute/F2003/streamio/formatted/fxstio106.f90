!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: cp $TR_SRC/check_array.inc .; cp $TR_SRC/check_interface.inc .; rm -f fxstio106.dat
! %COMPOPTS: 
! %GROUP:  fxstio106.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f check_array.inc check_interface.inc
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : I/O Stream Access Mode
!*
!*  PROGRAMMER                 : Bahram Chehrazy
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ, BACKSPACE
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test Integer Pointer, Fortran90 Pointer,
!*                               Pointee array and pointer array with formatted
!*                               synchronous stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/12/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 


  include 'check_array.inc'

  program fxstio106 

     implicit none
     integer           :: i, j, k, l, ios
     integer,parameter :: N = 10
     integer*4         :: i4_in, i4_out, pte_in, pte_out 
     real*4            :: r4_in(N), r4_out(N), pte_arr_in(N), pte_arr_out(N) 
     integer*4,pointer :: i4_pin, i4_pout
     integer*4,target  :: i4_tin, i4_tout
     real*8,pointer    :: r8_pin(:), r8_pout(:)
     real*8,target     :: r8_tin(N), r8_tout(N)

     logical precision_R4, precision_R8, precision_R6

     include 'check_interface.inc'

     pointer(ptr1_in, pte_in) 
     pointer(ptr1_out, pte_out) 
     pointer(ptr2_in, pte_arr_in) 
     pointer(ptr2_out, pte_arr_out) 

!********************************************************** 
!       Initialization                                    *
!********************************************************** 

     i4_in = 987654
     i4_tin = -2010571
     r4_in = 3.14
     r8_tin = -0.00000000009841

     i4_pin => i4_tin 
     i4_pout => i4_tout
     r8_pin => r8_tin
     r8_pout => r8_tout

     ptr1_in  = loc(i4_in)
     ptr1_out = loc(i4_out)
     ptr2_in  = loc(r4_in)
     ptr2_out = loc(r4_out)
     

!********************************************************** 
!       Writing and Reading the file                      *
!********************************************************** 

     OPEN(1, FILE='fxstio106.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='NEW', IOSTAT=ios, ERR=90)

!
!   Testing integer pointer
!
     WRITE(1, FMT='(2I10)', IOSTAT=ios, ERR=91) pte_in 
     BACKSPACE(1, IOSTAT=ios, ERR=93)
     READ(1, FMT='(I10)', IOSTAT=ios, ERR=92) pte_out

     WRITE(1, FMT='(10E8.3)', IOSTAT=ios, ERR=91) pte_arr_in 
     BACKSPACE(1, IOSTAT=ios, ERR=93)
     READ(1, FMT='(10E8.3)', IOSTAT=ios, ERR=92) pte_arr_out

!
!   Testing Fortran 90 Pointer arrays
!

     WRITE(1, FMT='(I8, 10D20.13)', IOSTAT=ios, ERR=91) i4_pin, r8_pin
     BACKSPACE(1, IOSTAT=ios, ERR=93)
     READ(1, FMT='(I8, 10D20.13)', IOSTAT=ios, ERR=92) i4_pout, r8_pout


!********************************************************** 
!        Checking the Results                             *
!********************************************************** 

     if ( pte_in .ne. pte_out .or. &
    &     i4_in .ne. i4_out  ) error stop 10

     if ( .not. Array_Check (pte_arr_in, pte_arr_out) .or. &
    &     .not. Array_Check (r4_in, r4_out)	) error stop 11

     if ( i4_tin .ne. i4_tout ) error stop 12

     if ( .not. Array_Check (r8_tin, r8_tout) ) error stop 13

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90 
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91 
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92 
93   print *, "Error in backspace: IOSTAT = ", ios
     error stop 93 

   end program
