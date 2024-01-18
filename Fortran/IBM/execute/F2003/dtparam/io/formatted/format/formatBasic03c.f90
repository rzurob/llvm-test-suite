!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatBasic03c.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 5 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. test READ statement with different control and data edit descriptor: bn,bz,iw.m ..
!* 2. unit number in READ statement is dummy argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len  :: l1
    integer      :: i1(l1-1:l1+1)
  end type

  type,extends(base) :: child(l2)
     integer,len  :: l2
     integer      :: i2(l1:l2)
  end type

  type,extends(child) :: gen3(l3)
     integer,len  :: l3
     type(child(l3-2,l3-1)) :: comp 
  end type

  contains

    subroutine readchild1(unit,arg)
       class(child(*,*)),intent(inout) :: arg(:)
       integer,intent(in) :: unit

       select type(arg)
         type is(child(*,*))
           read(unit,fmt='(3i5/2i5.3,bn,/3i5.1/2i5)') arg
         class default
           error stop 106_4
       end select
    end subroutine

    subroutine readchild2(unit,arg)
       class(child(*,*)),intent(inout) :: arg(:)
       integer,intent(in) :: unit

       select type(arg)
         type is(child(*,*))
           read(unit,fmt='(3i5.1/2i5)') arg
         class default
           error stop 107_4
       end select
    end subroutine

    subroutine readgen3(arg)
       class(base(*)),intent(inout) :: arg(:)

       select type(arg)
         type is(gen3(*,*,*))
           read(2*arg%l3,fmt=100) arg

         100 format(bn,3i5/2i5.3,bz,/,3i5/2i5.3) 
         class default
           error stop 108_4
       end select
    end subroutine

end module

program formatBasic03c
  use m
  implicit none

  class(base(3)),target,allocatable :: tbase1(:)
  class(base(:)),target,allocatable :: tbase2(:)
  class(base(:)),pointer :: pbase(:)=>null()
   
  integer :: ios

  allocate(gen3(3,4,5) :: tbase1(-1:0))

  allocate(gen3(3,4,5) :: tbase2(2:2))

  pbase(2:)=>tbase1

  open(10,file="formatBasic03c.in",action='read',access='sequential',&
       form='formatted',blank='zero',iostat=ios)

  if(ios /= 0) then
     print *,"fail to open the file, iostat=",ios
     return
  end if

  select type(tbase1)
    type is(gen3(*,*,*))
        call readchild1(10,tbase1%comp)
        print *,tbase1%comp
    class default
      error stop 101_4
  end select

  select type(tbase2)
    type is(gen3(*,*,*))
        call readchild2(10,tbase2%comp)
        print *,tbase2%comp
    class default
      error stop 102_4
  end select

  select type(pbase)
    type is(gen3(*,*,*))
        print *,pbase%comp
    class default
      error stop 103_4
  end select   

  pbase=>tbase2

  select type(pbase)
    type is(gen3(*,*,*))
        print *,pbase%comp
    class default
      error stop 104_4
  end select  

  rewind(10)

  select type(tbase2)
    type is(gen3(*,*,*))
        call readgen3(tbase2)
        print *,tbase2
    class default
      error stop 105_4
  end select
     
  close(10)
  
end program
