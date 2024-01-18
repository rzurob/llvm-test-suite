!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 3 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type mytype
      contains
         procedure,nopass :: proc2=>sub2
         generic :: proc=>proc2
   end type
   type dtp(l)
      integer,len :: l
   end type
   contains
      subroutine sub2(from,to)
         type(dtp(*)),allocatable       :: from(:)
         type(dtp(from%l)),allocatable  :: to(:)

         print *,"call sub2"
         call move_alloc(from,to)
      end subroutine
end module

program d357067

  use m
  implicit none

  integer   :: i
  type(mytype) :: mytype1

  type(dtp(7)),allocatable  :: from2(:)
  type(dtp(7)),allocatable  :: to2(:)

  allocate(from2(1:3))

  call sub2(from2(:),to2(:))
  call sub2(from2(1:3),to2(:))
  call mytype1%proc(from2(:),to2(:))
  call mytype1%proc(from2(1:3),to2(:))

end program

