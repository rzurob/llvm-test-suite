!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatBasic01c.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 3 2008 
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
!* 1. drived type has scalar intrinsic components
!* 2. use read statement with edit descriptor
!* 3. test pointer with non-deferred & deferred length parameter
!* 4. test dummy argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type :: base(k1,k2,l1,l2)
     integer,kind   :: k1,k2
     integer,len    :: l1,l2
     integer(k1)    :: i1
     real(2*k2)     :: r1
     complex(k1+k2) :: x1
     character(l1+l2) :: c1
     logical(k1)      :: log1 
  end type

  contains
    subroutine readbase(arg1,arg2)
       type(base(4,4,*,*)),pointer,intent(inout) :: arg1
       type(base(4,4,:,:)),pointer,intent(inout) :: arg2(:)

       if(associated(arg1) .and. associated(arg2)) then
          read (10,fmt= '(bn,i5,bz,2i3)') arg1%i1,arg2%i1
          read (10,fmt= '(3(f8.3))')   arg1%r1,arg2%r1
          read (10,fmt= '(f9.1,f7.2)') arg1%x1,arg2%x1
          read (10,fmt= '(a8,a10,a3)') arg1%c1,arg2%c1
          read (10,fmt= '(l2,l10,l5)') arg1%log1,arg2%log1
       else
          write(*,*) "dummy argument is not associated"
          stop 10
       end if

       write(*,*) " --in readbase-- "

       write(*,'(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l2)') arg1

       write(*,101) arg2

       read(10,fmt= '(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l2)')  arg1

       write(*,'(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l2)') arg1

       read(10,fmt= '(i3,f8.3,f9.1,f7.2,a10,l10)' ) arg2

       write(*,fmt= 101 ) arg2

101 format(2(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l3,/))

    end subroutine

end module

program formatBasic01c
  use m
  implicit none

  type(base(4,4,3,5)),pointer :: base1
  type(base(4,4,:,:)),pointer :: base2(:)

  integer        :: ios

  allocate(base(4,4,3,5) :: base1,base2(-1:0) )

  open(10,file="formatBasic01c.in",iostat=ios)
 
  if(ios .eq. 0) then
     read (10,fmt= '(bn,i5,bz,2i3)') base1%i1,base2%i1
     read (10,fmt= '(3(f8.3))')  base1%r1,base2%r1
     read (10,fmt= '(f9.1,f7.2)') base1%x1,base2%x1
     read (10,fmt= '(a8,a10,a3)') base1%c1,base2%c1
     read (10,fmt= '(l2,l10,l5)') base1%log1,base2%log1
  endif

  write(*,'(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l2)') base1

  write(*,100) base2

  read(10,fmt= '(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l2)')  base1

  write(*,'(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l2)') base1
 
  read(10,fmt= '(i3,f8.3,f9.1,f7.2,a10,l10)' ) base2

  write(*,fmt= 100) base2

  rewind 10

  call readbase(base1,base2)

  close(10)

100 format(2(i5,/,f8.3,/,f9.1,f7.2,/,a8,/,l3,/))

end program
