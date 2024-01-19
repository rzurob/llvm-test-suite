!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS SCALAR
!*  3. IF SOURCE IS SCALAR, EACH ELEMENT OF THE RESULT HAS A VALUE EQUAL TO SOURCE
!*  4. IF SOURCE IS SCALAR,THE SHAPE OF RESULT IS (MAX(NCOPIES,0)
!*  5. DERIVED TYPE HAS INTEGER SCALAR COMPONENT
!*  6. DEFECT 357385
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k)
      integer,kind :: k
      integer(k)   :: i
   end type
end module

program spreadSourceIsScalarIntComp01

  use m
  implicit none

  type(dtp(2))  :: dtp1
  type(dtp(4))  :: dtp2
  type(dtp(8))  :: dtp3

  type(dtp(2)),allocatable :: dtp5,result1(:)
  type(dtp(8)),allocatable :: result2(:)

  type(dtp(1)),pointer     :: dtp7
  type(dtp(4)),pointer     :: dtp8

  type(dtp(4)),target      :: tar=dtp(4)(i=8)

  dtp1=dtp(2)(i=1)
  dtp2=dtp(4)(i=2)
  dtp3=dtp(8)(i=3)

  associate(x=>spread(dtp1,1,3))
     print *,"dtp1--","shape:",shape(x),"param:",x%k,"value:",x
  end associate

  associate(x=>spread(dtp2,1,-1)) !--- zero-sized array--!
     print *,"dtp2--","shape:",shape(x),"param:",x%k,"value:",x
  end associate

  result2=spread(dtp3,1,1)
     print *,"dtp3--","shape:",shape(result2), &
             "param:",result2%k,"value:",result2

  associate(x=>spread(dtp(1)(i=4),1,3))
     print *,"dtp4--","shape:",shape(x),"param:",x%k,"value:",x
  end associate

  dtp5=dtp(2)(i=5)

  result1=spread(dtp5,1,2)
      print *,"dtp5--","shape:",shape(result1), &
                       "param:",result1%k,"value:",result1

  associate(x=>spread(dtp(8)(i=6),1,0)) !--- zero-size array--!
     print *,"dtp6--","shape:",shape(x),"param:",x%k,"value:",x
  end associate

  allocate(dtp7,source=dtp(1)(i=7) )

  associate(x=>spread(dtp7,1,3))
     print *,"dtp7--","shape:",shape(x),"param:",x%k,"value:",x
  end associate

  dtp8=>tar

  associate(x=>spread(dtp8,1,4))
     print *,"dtp8--","shape:",shape(x),"param:",x%k,"value:",x
  end associate


end program
