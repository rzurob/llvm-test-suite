!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of the ROUND= specifier
!*                               in I/O statements at compile time with -qlanglvl
!*                               set to a no F2003 level
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num=3.1415

  character(20) :: format_descrip1, format_descrip2, format_descrip3
  character(20) :: format_descrip4, format_descrip5, format_descrip6

  format_descrip1='(f6.4,rd)'

  format_descrip2='(f6.4,ru)'

  format_descrip3='(f6.4,rz)'

  format_descrip4='(f6.4,rn)'

  format_descrip5='(f6.4,rc)'

  format_descrip6='(f6.4,rp)'

  call setrteopts("langlvl=95std")

  open(UNIT=2,FILE='real4.dat', ROUND='zero')

  !round edit descriptors are not allowed when
  !langlvl runtime option is set to 95std

  read(2,format_descrip1) num

  write(*, format_descrip2) num

  print format_descrip3, num

   read(2,format_descrip4) num

  write(*, format_descrip5) num

  print format_descrip6, num

  close(2)

end program

