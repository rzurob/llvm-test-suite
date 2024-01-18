!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyTarget03.f
!*
!*  DATE                       : Nov. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1.actual target and actual pointer passed through dummy arguments,dummy pointer is associated with dummy target, after executing procedure, actual pointer will assocaite with actual target.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
     character(l1) :: c1(k1)
   end type
end module

program dummyArgDeferNonPolyTarget03
  use m
  implicit none

  type(dtp(2,:)),allocatable,target :: tar1
  type(dtp(2,:)),pointer            :: ptr1=>null()

  type(dtp(2,:)),allocatable,target :: tar2(:)
  type(dtp(2,:)),pointer            :: ptr2(:)=>null()

  call associate1(tar1,ptr1)
  if(.not. associated(ptr1,tar1))                         error stop 10_4
  if(ptr1%l1 /= 3)                                        error stop 11_4
  if(any(ptr1%c1 /= ["ab","cd"]))                         error stop 12_4
  if(tar1%l1 /= 3)                                        error stop 13_4
  if(any(tar1%c1 /= ["ab","cd"]))                         error stop 14_4

  tar1%c1=["12","34"]

  call associate1(tar1,ptr1)

  if(.not. associated(ptr1,tar1))                         error stop 15_4
  if(ptr1%l1 /= 3)                                        error stop 16_4
  if(any(ptr1%c1 /= ["12","34"]))                         error stop 17_4
  if(tar1%l1 /= 3)                                        error stop 18_4
  if(any(tar1%c1 /= ["12","34"]))                         error stop 19_4

  call associate2(tar2,ptr1)
  if(ptr1%l1 /= 3)                                        error stop 20_4
  if(any(ptr1%c1 /= ["ef","gh"]))                         error stop 21_4

  deallocate(tar2)

  call associate3(tar2,ptr2)
  if(ptr2%l1 /= 3)                                        error stop 22_4
  if(lbound(ptr2,1) /= -1)                                error stop 23_4
  if(ubound(ptr2,1) /= 0)                                 error stop 24_4
  if(any(ptr2(-1)%c1 /= ["ab","cd"]))                     error stop 25_4
  if(any(ptr2(0)%c1 /= ["ef","gh"]))                      error stop 26_4

  tar2=[dtp(2,3)(["00","11"]),dtp(2,3)(["22","33"])]

  call associate3(tar2,ptr2)

  if(any(ptr2(-1)%c1 /= ["00","11"]))                     error stop 27_4
  if(any(ptr2(0)%c1 /= ["22","33"]))                      error stop 28_4

  contains

   subroutine associate1(tar,ptr)
      type(dtp(2,:)),allocatable,target    :: tar
      type(dtp(2,:)),pointer,intent(out)   :: ptr

      if(.not. allocated(tar)) then
         allocate(tar,source=dtp(2,3)(["ab","cd"]))
      end if
      ptr=>tar

   end subroutine

   subroutine associate2(tar,ptr)
      type(dtp(2,:)),allocatable,target    :: tar(:)
      type(dtp(2,:)),pointer,intent(out)   :: ptr

      if(.not. allocated(tar)) then
         allocate(tar(-1:0),source= &
                     [dtp(2,3)(["ab","cd"]), dtp(2,3)(["ef","gh"])] )
      end if

      ptr=>tar(0)
   end subroutine

   subroutine associate3(tar,ptr)
      type(dtp(2,:)),allocatable,target    :: tar(:)
      type(dtp(2,:)),pointer,intent(out)   :: ptr(:)

      if(.not. allocated(tar)) then
         allocate(tar(-1:0),source= &
                     [dtp(2,3)(["ab","cd"]), dtp(2,3)(["ef","gh"])] )
      end if

      ptr=>tar
   end subroutine

end program
