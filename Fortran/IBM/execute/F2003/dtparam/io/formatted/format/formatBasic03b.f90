!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. derived type is polymorphic type,which has ultimate integer component
!* 2. test WRITE statement for dummy argument
!* 3. use different edit descriptor such as sp,ss,iw.m,iw etc
!* 4. use sign= specifier in open statement
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len  :: l1
    integer      :: i1(l1-1:l1+1)=1
  end type

  type,extends(base) :: child(l2)
     integer,len  :: l2
     integer      :: i2(l1:l2)=2
  end type

  type,extends(child) :: gen3(l3)
     integer,len  :: l3
     type(child(l3-2,l3-1)) :: comp
  end type

  contains

     subroutine writetype(arg)
        class(base(*)),intent(in) :: arg(:)
        select type(arg)
           type is(gen3(*,*,*))
              write(10,'(ss,t5,5i5,/,t5,5i5)') arg
              write(10,'(t5,5i5,/,t5,5i5)') arg
              write(10,'(ss,t3,5i7.4,/,t3,5i7.4)') arg
              write(10,'(t3,5i7.4,/,t3,5i7.4)') arg
           class default
              error stop 100_4
        end select
     end  subroutine
end module

program formatBasic03b
  use m
  implicit none

  class(base(3)),target,allocatable :: base1(:)
  class(base(:)),pointer :: base2(:)
  integer :: ios

  allocate(base1(2),source= &
    [gen3(3,4,5)(i1=[11,12,13],i2=[14,15], &
    comp=child(3,4)(i1=[-11,-12,-13],i2=[-14,-15])) , &
    gen3(3,4,5)(i1=[23,24,25],i2=[26,27], &
     comp=child(3,4)(i1=[-23,-24,-25],i2=[-26,-27])) ] )

  base2(0:)=>base1

  open(10,file="formatBasic03b.out",blank='null',sign='plus', &
         action='write',iostat=ios)

  if(ios .eq. 0) then
     call writetype(base1)
     call writetype(base2)
  else
     write(10,'(a,i4)' )  &
        "fail to open the file, iostat=",ios
     error stop 101_4
  end if

  close(10)

end program
