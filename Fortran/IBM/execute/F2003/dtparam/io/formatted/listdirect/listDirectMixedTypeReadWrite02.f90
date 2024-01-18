!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectMixedTypeReadWrite02.f
!*
!*  DATE                       : Jan. 19 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. read data with list directed IO, and write output with format specification
!* 2. use user defined operator(+)
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
  type DT(k0,l0)
     integer,kind :: k0 !k0=4
     integer,len  :: l0 !l0=5

     integer(k0)  :: i(l0)=-99
  end type
  type base(k1,l1)
    integer,kind :: k1 !k1=4
    integer,len  :: l1 !l1=3

    character(l1) :: c(k1)="***"
  end type
end module

module m2
  use m1
  type,extends(base) ::  child(k2,l2)
     integer,kind :: k2 !k2=4
     integer,len  :: l2 !l2=5

     logical(k2)  :: g(l2)=.false.
     type(DT(k2,l2)) :: dtcomp
  end type

  interface operator (+)
     module procedure plus1
  end interface

  contains

  function plus1(t1,t2)
    class(base(4,:)),allocatable,intent(in) :: t1(:)
    type(child(4,*,4,*)),intent(in) :: t2
    class(base(4,:)),allocatable :: plus1(:)
    integer :: i
     select type(t1)
        type is(child(4,*,4,*))
           allocate(child(t1%k1,2*t1%l1,t1%k2,t1%l2) :: plus1(size(t1)) )
           select type(x=>plus1)
              type is(child(4,*,4,*))
                  do i=1,size(t1)
                    x(i)%c=t1(i+1)%c//t2%c
                    x(i)%g=t1(i+1)%g .and. t2%g
                    x(i)%dtcomp%i=t1(i+1)%dtcomp%i + t2%dtcomp%i
                  end do
              class default
                  stop 13
           end select
        class default
           stop 12
     end select

  end function

  subroutine readData1(dt,unit)
     class(base(4,:)),allocatable,intent(out) :: dt(:)
     integer,intent(in) :: unit

     allocate(child(4,3,4,5) :: dt(2:3))
     select type(dt)
         type is(child(4,*,4,*))
            read(unit,*,decimal='comma') dt(2)
            read(unit,*,decimal='point') dt(3)
         class default
            stop 11
     end select

  end subroutine

end module

program listDirectMixedTypeReadWrite02
  use m2

  integer :: ios,i
  character(256) :: msg

  class(base(4,:)),allocatable :: b1(:),result(:)

  type(child(4,3,4,5)) :: b2

  open(10,file='listDirectMixedTypeReadWrite02.dat',&
       iostat=ios,iomsg=msg)

  if(ios <> 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10

  end if

  ! following is the input data
  !; 'a''b' 'A"B'; 'abc'
  !.false.    fall tru .tee ; feel
  !10 1*-10  ;     ; -11 /
  !"a'b" , "c""d" 1*, 1*ABC ,
  !T  1*.F 1* 1*.T 1*t
  !1* , -22 1*33 , , 9, 40*-1 /40*-1 is ignored
  !XLF 1*IBM , , G'H
  !2*F 3*.true. 2*77 , 1*-2 1*, 33 , ,/

  call readData1(b1,10)

  read(10,*) b2

  allocate(result(-1:0),source = b1 + b2 )

  ! output results with format descriptor
  select type(result)
    type is(child(4,*,4,*))
      do i=lbound(result,1),ubound(result,1)
        write(*,fmt=100)  result(i)%c
        write(*,fmt='(5(l2,:,","))' ) result(i)%g
        write(*,'(5(i5,:,","))' ) result(i)%dtcomp%i
      end do
    class default
      stop 14
  end select

100 format (4(a6,:,','))

  close(10)

end program
