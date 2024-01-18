!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadDiagRangeOfDIM01.f
!*
!*  DATE                       : Oct. 16 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. TYPE OF DIM SHALL BE INTEGER SCALAR WITH VALUE IN THE RANGE 1 <= DIM <= N+1,WHERE N IS THE RANK OF SOURCE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
    integer,kind :: k=2
    integer,len  :: l=3
    integer(k)   :: i(l)=1
  end type
end module

program spreadDiagRangeOfDIM01

  use m
  implicit none

  type(dtp(2,3)) :: dtp1
  type(dtp(4,2)) :: dtp2=dtp(4,2)(i=[1,2])
  type(dtp(2,3)) :: dtp3(4)
  type(dtp(2,:)),allocatable :: dtp4(:,:)

  allocate(dtp4(2,2),source=reshape(dtp3,(/2,2/)))

  print *,spread(dtp1,dim=2,ncopies=5)
  print *,spread(dtp2,dim=100,ncopies=100)
  print *,spread(dtp3,dim=3,ncopies=1)
  print *,spread(dtp4,dim=4,ncopies=10)
  print *,spread(dtp1,dim=-2,ncopies=5)
  print *,spread(dtp2,dim=0,ncopies=100)
  print *,spread(dtp3,dim=-600,ncopies=100)
  print *,spread(dtp4,dim=-3,ncopies=10)

end program

