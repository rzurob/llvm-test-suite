!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign069.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ptrassign069
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

real, pointer :: ptr1(:)
real, target  :: tar1(100)

interface 
  subroutine ptrassign(ptr,tar)
    real, pointer :: ptr(:)
    real, target  :: tar(:)
  end subroutine
end interface

procedure(ptrassign), pointer :: procptr

procptr=>ptrassign

do i=1,100
  tar1(i)=100
end do

call procptr(ptr1, tar1)

if(lbound(ptr1, dim=1).ne. 10) error stop 5
if(ubound(ptr1, dim=1).ne. 109) error stop 6
if(any(shape(ptr1).ne.(/100/))) error stop 7
if(.not.associated(ptr1,tar1)) error stop 8

end


subroutine ptrassign(ptr,tar)
  real, pointer :: ptr(:)
  real, target  :: tar(:)
  
  ptr(10:)=>tar
  
  if(lbound(ptr, dim=1).ne. 10) error stop 1
  if(ubound(ptr, dim=1).ne. 109) error stop 2
  if(any(shape(ptr).ne.(/100/))) error stop 3
  if(.not.associated(ptr,tar)) error stop 4
  
  
end subroutine
  
  