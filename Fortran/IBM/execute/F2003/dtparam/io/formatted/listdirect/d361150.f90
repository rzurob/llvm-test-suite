!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361150.f
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
program d361150

  complex(4)  :: x1=(0.,0.)
  complex(8)  :: x2=(0.,0.)
  complex(16) :: x3=(0.,0.)
  logical,external :: precision_x8,precision_x6,precision_x3

  open(10,file='d361150.dat')
  read(10,*) x1,x2,x3

  if(.not. precision_x8(x1,(1.2_4,1.2_4)))     stop 1
  if(.not. precision_x6(x2,(1.2_8,1.2_8)))     stop 2
  if(.not. precision_x3(x3,(1.2_16,1.2_16)))   stop 3

  close(10)

end program
