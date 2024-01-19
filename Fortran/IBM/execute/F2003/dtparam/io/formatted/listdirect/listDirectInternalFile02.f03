!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 23 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test read records into internal file and read data out of internal file
!* 2. Internal file is default allocatable character array
!* 3. Derived type has 2 or 3 dimensional array components
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
    type A(k1,l1)
      integer, kind :: k1 !k1=2
      integer, len  :: l1 !l1=3

      character(k1) :: c1(l1-1,l1-1)="**"
      integer(k1)   :: i1(l1-1,l1-1,l1-1)=-99
    end type

end module

module m2
  use m1

  type B(k2,l2)
     integer,kind :: k2 ! k2=2
     integer,len  :: l2 ! l2=3

     logical(k2)   :: g1(k2:l2,k2:l2)=.false.
     complex(2*k2) :: x1(l2:l2+1,l2:l2+1)=(0.,0.)
     type(A(k2,l2))     :: a1comp
     type(A(k2,k2+1))  :: a2comp

  end type

  contains

      subroutine readIntoBuffer(unit,buffer)
          integer,intent(in) :: unit
          character(*),intent(inout) :: buffer(:)
          integer :: mypos

          inquire(unit,pos=mypos)
          read(unit,*,pos=mypos) buffer(lbound(buffer,1) )

          inquire(unit,pos=mypos)
          if(mypos /= 21)                  error stop 11

          read(unit,*,pos=mypos) buffer(lbound(buffer,1) + 1)
          inquire(unit,pos=mypos)
          if(mypos /=93)                   error stop 12

          read(unit,*,pos=mypos) buffer(lbound(buffer,1) + 2)
          inquire(unit,pos=mypos)
          if(mypos /= 118)                 error stop 13

          read(unit,*,pos=mypos) buffer(lbound(buffer,1) + 3)
          inquire(unit,pos=mypos)
          if(mypos /= 155)                 error stop 14

          read(unit,*,pos=mypos) buffer(lbound(buffer,1) + 4)
          inquire(unit,pos=mypos)
          if(mypos /= 186)                 error stop 15

          read(unit,*,pos=mypos) buffer(lbound(buffer,1) + 5)
          inquire(unit,pos=mypos)
          if(mypos /= 221)                 error stop 16

      end subroutine

      subroutine readOutOfBuffer(buffer,dt)
          character(*),intent(in) :: buffer(:)
          type(B(2,*)),intent(inout) :: dt

          read(buffer(lbound(buffer,1)),*)    dt%g1

          read(buffer(lbound(buffer,1)+1),*)  dt%x1

          read(buffer(lbound(buffer,1)+2),*)  dt%a1comp%c1

          read(buffer(lbound(buffer,1)+3),*,decimal='comma')  dt%a1comp%i1

          read(buffer(lbound(buffer,1)+4),*)  dt%a2comp%c1

          read(buffer(lbound(buffer,1)+5),*)  dt%a2comp%i1

      end subroutine

end module

program listDirectInternalFile02
  use m2
  implicit none

  integer :: ios
  character(256) :: msg
  character(:),allocatable :: buffer(:)
  logical,external :: precision_x8

  type(B(2,:)) ,allocatable :: b1

  allocate(B(2,3) :: b1)

  allocate(character(150) :: buffer(3:8))

  open(10,file='listDirectInternalFile02.dat', form='formatted',&
        access='stream',status='old',iostat=ios,iomsg=msg)

  if(ios <> 0) then
     print *,"fail to open the file "
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10
  end if

  ! following is the record we want to read

  !'T,, .true.  .fall'
  !"(-2.1, +3.4) 1*(-5.6E-10, 8.99E2 ), (-3.1E-02, 0.5E-3 ) (0.1 , -1.2 )"
  !", ABC 'abc', ""XLF"" "
  !' -3 ; -56 ; ; 2*-4  1* ; +23 -112 '
  !' ''blue'', "red" , test IBM '
  !" , 1*-10  -5 +22 , , -78, +56 / "

  call readIntoBuffer(10,buffer)
  call readOutOfBuffer(buffer,b1)

  ! verify results

  if(b1%g1(2,2) .neqv. .true.)                          error stop 17
  if(b1%g1(3,2) .neqv. .false.)                         error stop 18
  if(b1%g1(2,3) .neqv. .true.)                          error stop 19
  if(b1%g1(3,3) .neqv. .false.)                         error stop 20

  if(.not. precision_x8(b1%x1(3,3),(-2.1_4,3.4_4)))     error stop 21
  if(.not. precision_x8(b1%x1(4,3),(-5.6E-10,8.99E2)))  error stop 22
  if(.not. precision_x8(b1%x1(3,4),(-3.1E-02,0.5E-3)))  error stop 23
  if(.not. precision_x8(b1%x1(4,4),(0.1,-1.2)))         error stop 24

  if(b1%a1comp%c1(1,1) /= "**")                         error stop 25
  if(b1%a1comp%c1(2,1) /= "AB")                         error stop 26
  if(b1%a1comp%c1(1,2) /= "ab")                         error stop 27
  if(b1%a1comp%c1(2,2) /= "XL")                         error stop 28

  if(b1%a1comp%i1(1,1,1) /= -3)                         error stop 29
  if(b1%a1comp%i1(2,1,1) /= -56)                        error stop 30
  if(b1%a1comp%i1(1,2,1) /= -99)                        error stop 31
  if(b1%a1comp%i1(2,2,1) /= -4)                         error stop 32
  if(b1%a1comp%i1(1,1,2) /= -4)                         error stop 33
  if(b1%a1comp%i1(2,1,2) /= -99)                        error stop 34
  if(b1%a1comp%i1(1,2,2) /= +23)                        error stop 35
  if(b1%a1comp%i1(2,2,2) /= -112)                       error stop 36

  if(b1%a2comp%c1(1,1) /= "bl")                         error stop 37
  if(b1%a2comp%c1(2,1) /= "re")                         error stop 38
  if(b1%a2comp%c1(1,2) /= "te")                         error stop 39
  if(b1%a2comp%c1(2,2) /= "IB")                         error stop 40

  if(b1%a2comp%i1(1,1,1) /= -99)                        error stop 41
  if(b1%a2comp%i1(2,1,1) /= -10)                        error stop 42
  if(b1%a2comp%i1(1,2,1) /= -5)                         error stop 43
  if(b1%a2comp%i1(2,2,1) /= +22)                        error stop 44
  if(b1%a2comp%i1(1,1,2) /= -99)                        error stop 45
  if(b1%a2comp%i1(2,1,2) /= -78)                        error stop 46
  if(b1%a2comp%i1(1,2,2) /= 56)                         error stop 47
  if(b1%a2comp%i1(2,2,2) /= -99)                        error stop 48

  close(10)

end program
