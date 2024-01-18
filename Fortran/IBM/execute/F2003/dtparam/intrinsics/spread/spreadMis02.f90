!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadMis02.f
!*
!*  DATE                       : Oct. 23 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. RESULT OF SPREAD IS A ZERO-SIZED ARRAY WHEN NCOPIES HAS VALUE OF ZERO
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dt(k,l)
     integer,kind :: k
     integer,len  :: l
     integer(k)   :: i(l-1:l+1)
  end type
end module

program spreadMis02
  use m
  implicit none

  type(dt(2,3)) :: dt1=dt(2,3)(i=[1,2,3])
  type(dt(2,:)),allocatable :: dt2(:) , dt3(:,:)

  if(any(shape(spread(dt1,1,0)) /= 0))               stop 1
  if(lbound(spread(dt1,1,0),1) /= 1)                 stop 2
  if(ubound(spread(dt1,1,0),1) /= 0)                 stop 3

  dt2=spread(dt1,1,2)

  if(any(shape(spread(dt2,1,0)) /= [0,2]))           stop 4
  if(lbound(spread(dt2,1,0),1) /= 1)                 stop 5
  if(ubound(spread(dt2,1,0),1) /= 0)                 stop 6

  if(any(shape(spread(dt2,2,0)) /= [2,0]))           stop 7
  if(lbound(spread(dt2,2,0),2) /= 1)                 stop 8
  if(ubound(spread(dt2,2,0),2) /= 0)                 stop 9

  dt3=spread(dt2,1,3)

  if(any(shape(spread(dt3,1,0)) /= [0,3,2]))         stop 10
  if(lbound(spread(dt3,1,0),1) /= 1)                 stop 11
  if(ubound(spread(dt3,1,0),1) /= 0)                 stop 12

  if(any(shape(spread(dt3,2,0)) /= [3,0,2]))         stop 13
  if(lbound(spread(dt3,2,0),2) /= 1)                 stop 14
  if(ubound(spread(dt3,2,0),2) /= 0)                 stop 15

  if(any(shape(spread(dt3,3,0)) /= [3,2,0]))         stop 16
  if(lbound(spread(dt3,3,0),3) /= 1)                 stop 17
  if(ubound(spread(dt3,3,0),3) /= 0)                 stop 18


end program
