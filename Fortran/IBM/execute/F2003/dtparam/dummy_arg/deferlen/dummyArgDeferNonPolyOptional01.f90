!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyOptional01.f
!*
!*  DATE                       : Nov. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. one of dummy argument is allocatable or pointer derived type, another dummy argument has optional  attribute,  allocate different component value depends on optional arguments presenting or not.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k1,l1)
     integer,kind    :: k1
     integer,len     :: l1
     integer(k1)     :: id(l1-1:l1+1)
     character(l1)   :: name(l1-1:l1+1)
  end type

end module

program dummyArgDeferNonPolyOptional01
  use m
  implicit none

  type(dtp(2,:)),allocatable :: dtp1
  type(dtp(2,:)),pointer     :: dtp2=>null()
  character(6),allocatable   :: name(:)

  name=[character(6) :: "Robert","Tom","Lisa"]

  allocate(dtp1,source= dtp(2,6)([1,2,3],name) )

  call sub1(dtp1,name)

  if(any(dtp1%id /= [1,2,3]))                          error stop 10_4
  if(any(dtp1%name /= ["Robert","Tom   ","Lisa  "]))   error stop 11_4

  call sub1(dtp1)
  if(any(dtp1%id /= [1,2,3]))                          error stop 12_4
  if(any(dtp1%name /= ["uname1","uname2","uname3"]))   error stop 13_4

  call sub2(dtp2,name)

  if(any(dtp2%id /= [1,2,3]))                          error stop 14_4
  if(any(dtp2%name /= ["Robert","Tom   ","Lisa  "]))   error stop 15_4

  call sub2(dtp2)
  if(any(dtp2%id /= [1,2,3]))                          error stop 16_4
  if(any(dtp2%name /= ["uname1","uname2","uname3"]))   error stop 17_4

  contains

     subroutine sub1(dtp,name)
        type(dtp(2,:)),allocatable :: dtp
        character(6),optional,intent(in) :: name(3)

        if(present(name))  then
           if(allocated(dtp))  deallocate(dtp)
           allocate(dtp,source= &
               dtp(2,6)([1,2,3],name) )
        else
           if(allocated(dtp))  deallocate(dtp)
           allocate(dtp,source= &
               dtp(2,6)([1,2,3],["uname1","uname2","uname3"]) )
        end if

     end subroutine

     subroutine sub2(dtp,name)
        type(dtp(2,:)),pointer :: dtp
        character(6),optional,intent(in) :: name(3)

        if(present(name))  then
           allocate(dtp,source= &
               dtp(2,6)([1,2,3],name) )
        else
           allocate(dtp,source= &
               dtp(2,6)([1,2,3],["uname1","uname2","uname3"]) )
        end if

     end subroutine

end program
