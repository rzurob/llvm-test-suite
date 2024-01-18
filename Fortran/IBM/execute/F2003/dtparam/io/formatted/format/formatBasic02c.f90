!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatBasic02c.f
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
!*  1. derived type has multiple character scalar and array component.
!*  2. test READ statement for dummy argument with assumed length parameter
!*  3. use different edit descriptors
!*  4. read input rules
!* Let len be the length of the input/output list item. If the specified field width w for an A edit descriptor corresponding to an input item is greater than or equal to len,the rightmost len characters will be taken from the input field. If the specified field width w is less than len, the w characters will appear left justified with lenw trailing blanks in the internal value.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1,l2)
     integer,len :: l1,l2
     character(l1) :: c1
     character(l2) :: c2(l1:l2)
     character(l1+l2) :: c3(l1:l2)
  end type

   contains

       subroutine readbase1(arg)
          type(base(*,*)),allocatable,intent(inout) :: arg(:)

          read(10,'(2(a2,2a3,2a5))') arg
       end subroutine

       subroutine writebase1(arg)
          type(base(*,*)),allocatable,intent(inout) :: arg(:)
          integer :: i

          do i=lbound(arg,1),ubound(arg,1)
             write(*,*) "|",arg(i)%c1,"|"
             write(*,*) "|",arg(i)%c2,"|"
             write(*,*) "|",arg(i)%c3,"|"
          end do
       end subroutine

       subroutine readbase2(arg)
          type(base(*,*)),allocatable,intent(inout) :: arg(:)
          ! w in aw is different from readbase1,length input field may greater, or equal, or less than length specified in edit descriptor
          read(10,'(2(a4,2a2,a3,a7))') arg
       end subroutine

       subroutine readbase3(arg)
          type(base(*,*)),allocatable,intent(inout) :: arg(:)
          ! when format is exhausted, new record is taken
          read(10,'((a1,a3,a5,a4,a4))') arg
       end subroutine

       subroutine readbase4(arg)
          type(base(*,*)),allocatable,intent(inout) :: arg(:)
          ! use slash editing in edit descriptor
          read(10,'(2(bn,a3,/,a2,a4,/,a4,a1))' ) arg
       end subroutine

       subroutine readbase5(arg)
          type(base(*,*)),allocatable,intent(inout) :: arg(:)
          ! reread last input field
          read(10,'(2(a2,2(a3,a4)) )' ) arg
       end subroutine

end module

program formatBasic02c
  use m
  implicit none

  type(base(2,3)),allocatable :: base(:)
  integer :: ios

  allocate(base(2,3) :: base(0:1))

  open(10,file='formatBasic02c.in',action='read', iostat=ios)

  if(ios .eq. 0) then
     write(*,*) "---test 1---"
     call readbase1(base)
     call writebase1(base)
     rewind(10)
     write(*,*) "---test 2---"
     call readbase2(base)
     call writebase1(base)
     write(*,*) "---test 3---"
     call readbase3(base)
     call writebase1(base)
     write(*,*) "---test 4---"
     call readbase4(base)
     call writebase1(base)
     backspace(10)
     write(*,*) "---test 5---"
     call readbase5(base)
     call writebase1(base)
  else
     print *,"iostat is not zero,fail to open the file"
     print *,"iostat=",ios
     stop 11
  end if

end program
