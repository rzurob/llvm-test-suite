!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 16 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,NC,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. TYPE OF NCOPIES SHALL BE INTEGER SCALAR
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
    integer,kind :: k
    integer,len  :: l
    integer(k)   :: i(l)
  end type
end module

program spreadDiagTypeOfNC01

  use m
  implicit none

  type(dtp(1,2)) :: dtp1=dtp(1,2)(i=[1,1])
  type(dtp(1,:)),allocatable :: dtp2(:)

  allocate(dtp2(2),source=[dtp(1,2)(i=[1,1]),dtp(1,2)(i=[2,2])] )
  print *,spread(dtp1,1,[1,1])
  print *,spread(dtp2,2,dtp2)
  print *,spread(dtp1,1,0.5)
  print *,spread(dtp2,2,.false.)
  print *,spread(dtp2,2,dtp1)

end program

