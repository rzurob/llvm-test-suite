!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: 
! %GROUP:  fxstio309.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : I/O Stream Access
!*
!*  PROGRAMMER                 : Bahram Chehrazy
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test ACCESS= specifier in OPEN stmt with
!*				 diferent character experession in stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  04/04/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  program fxstio309 

     implicit none
     integer    i, ios
     integer, parameter :: N = 10
     character*21 ch21_out /"This is a stream file"/ , ch21_in

     character*6          :: access_var         
     character*6,parameter:: access_par='Stream' ! Parameter as access variable   
     character*6, pointer :: access_pointer    ! F90 pointer as access variable 
     character*6, target  :: access_target
     character*6          :: access_pte
     character*6, target  :: access_array(N)   ! Array elements as access variable
     character*6, pointer :: access_arr_ptr(:) ! Array pointer as access variable

     pointer(access_ptr, access_pte)          ! Integer pointer as access variable

	
!********************************************************** 
!        Initialization of variables                      *
!********************************************************** 

     access_var = "sTrEaM"
     access_target = "STREAM" 
     access_pointer => access_target		
     access_ptr = LOC(access_var) 
     access_array(N-1) = "stream"
     access_array(N) = 'STReam'
     access_arr_ptr => access_array(N-1:N) 

!********************************************************** 
!    Testing character variable as access specifier       *
!********************************************************** 

     OPEN(1, FORM='FORMATTED', ACCESS=access_var, &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(A)', POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1, FMT='(A)', POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 21

     CLOSE(1)

!********************************************************** 
!    Testing parameter as access specifier                *
!********************************************************** 

     OPEN(1, FORM='FORMATTED', ACCESS=access_par, &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(A)', POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1, FMT='(A)', POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 22

     CLOSE(1)

!********************************************************** 
!    Testing Fortran90 pointer as access specifier        *
!********************************************************** 

     OPEN(1, FORM='UNFORMATTED', ACCESS=access_pointer, &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1,  POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 23

     CLOSE(1)

!********************************************************** 
!    Testing Integer pointer as access specifier          *
!********************************************************** 

     OPEN(1, FORM='UNFORMATTED', ACCESS=access_pte, &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1,  POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 24

     CLOSE(1)

!********************************************************** 
!    Testing Array element as access specifier            *
!********************************************************** 

     OPEN(1, FORM='UNFORMATTED', ACCESS=access_array(N), &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1,  POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 25

     CLOSE(1)

!********************************************************** 
!    Testing Array pointer as access specifier            *
!********************************************************** 

     OPEN(1, FORM='UNFORMATTED', ACCESS=access_arr_ptr(1), &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1,  POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 26

     CLOSE(1)

!********************************************************** 
!    Testing character concatenation in access specifier  *
!********************************************************** 

     OPEN(1, FORM='UNFORMATTED', ACCESS="Str"//"eam", &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1,  POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 27

     CLOSE(1)

!********************************************************************** 
! Testing character constant with trailing blanks as access specifier *
!********************************************************************** 

     OPEN(1, FORM='UNFORMATTED', ACCESS="StreaM    ", &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, POS=10, IOSTAT=ios, ERR=91) ch21_out 
     READ(1,  POS=10, IOSTAT=ios, ERR=92) ch21_in

     if ( ch21_in .ne. ch21_out ) error stop 28

     CLOSE(1)




     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90 
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91 
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92 

   end program
