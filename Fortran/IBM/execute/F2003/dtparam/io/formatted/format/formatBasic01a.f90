!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatBasic01a.f
!*
!*  DATE                       : Dec. 3 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. drived type has scalar intrinsic components
!* 2. use print statement with edit descriptor
!* 3. test allocatable with non-deferred & deferred length parameter
!* 4. test function result, dummy argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type :: base(k1,k2,l1,l2)
     integer,kind   :: k1,k2
     integer,len    :: l1,l2
     integer(k1)    :: i1=1
     real(2*k2)       :: r1=1.1
     complex(k1+k2) :: x1=(+2.2,-3.3)
     character(l1+l2) :: c1="xlf"
     logical(k1)      :: log1=.true.
  end type

    contains

       elemental function getResult(arg)
          type(base(4,4,*,*)),intent(in) :: arg
          type(base(4,4,3,5)) :: getResult

          getResult=arg

       end function
end module

program formatBasic01a
  use m
  implicit none

  type(base(4,4,3,5)),allocatable :: base1
  type(base(4,4,3,5)),allocatable :: base2(:)


  allocate(base1,source= &
          base(4,4,3,5)( i1=-1, r1=-1.1 , x1=(-4.4,+5.5), &
          c1="fortran test",log1=.false. ) )

  allocate(base(4,4,3,5) :: base2(-1:0) )

  base2=[base(4,4,3,5)(),base1]


  print 100, base1
  print 100, base1%i1,base1%r1,base1%x1,base1%c1,base1%log1
  print 100, getResult(base1)
  call checkResult1(base1)

  print *
  print 100,base2(-1)
  print 100,base2(-1)%i1,base2(-1)%r1,base2(-1)%x1,base2(-1)%c1,base2(-1)%log1
  print 100,getResult(base2(-1))
  call checkResult1(base2(-1))

  print *
  print '(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3)', base2(0)
  print '(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3)', &
        base2(0)%i1,base2(0)%r1,base2(0)%x1,base2(0)%c1,base2(0)%log1
  print '(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3)',getResult(base2(0))
  call checkResult1(base2(0))

  print *
  print '(2(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3,/))', base2
  print '(2(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3,/))', getResult(base2)
  call checkResult2(base2)

100 format(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3)

  contains

      subroutine checkResult1(arg)
        type(base(4,4,*,*)),intent(in) :: arg

        print '(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3)', arg
      end subroutine

      subroutine checkResult2(arg)
        type(base(4,4,*,*)),allocatable,intent(in) :: arg(:)

        print '(2(1X,i4,1X,f10.5,1X,2(f8.3),1X,a12,1X,l3,/))', arg
      end subroutine

end program
