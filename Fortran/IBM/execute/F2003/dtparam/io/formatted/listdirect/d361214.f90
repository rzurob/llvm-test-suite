!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361214.f
!*
!*  DATE                       : Jan. 20 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!234567890123456789012345678901234567890123456789012345678901234567890
program d361214

  character(3) :: c(3)="***"
  open(10,file='d361214.dat',decimal='comma')

  read(10,*) c
  print *,c

  close(10)

end program

